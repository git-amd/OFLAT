(* OCamlFlatSupport.ml - AMD/2019 *)

open Yojson.Basic.Util


(* --- Error --- *)

module Error = struct
	type t = string list ref

	let sink: t = ref []

	let err: t ref = ref sink

	let start () =	(* setup a log of errors *)
		let r = ref [] in
			err := r;
			r        (* returns a new error log *)

	let stop () = (* disables logging *)
		sink := [];
		err := sink

	let error (culprit: string) (str: string) (res: 'a): 'a =
		let r = !err in
			r := !r@[culprit ^ ": " ^ str]; res

	let show (expectedKind: string) (name: string): unit =
		let r = !err in
			if !r = [] then ()
			else (
				print_string (expectedKind^" "^name^ " has errors:\n");
				List.iter (fun m -> print_string ("    "^m^"\n")) !r
			)
end


(* --- Util --- *)

module Util =  struct
	let load_file (filename: string): string =
		try
			let ic = open_in filename in
			let n = in_channel_length ic in
			let s = Bytes.create n in
				really_input ic s 0 n;
				close_in ic;
				(Bytes.to_string s)
		with
			Sys_error str ->
				Error.error "file" str ""
	;;

	let println (str: string) =
		print_string str ; print_newline()

	let header (str: string) =
		println "----------" ; println str

	let printStates (st:string list) =
	    List.iter (fun x -> print_string x; print_string ", ") st

	let printWord (w:char list) =
	    print_char '\'' ; List.iter (fun c -> print_char c ) w; println "'"

	let printWordList (l:char list list) =
	    List.iter printWord l

	let addAll symb = List.map (fun l -> symb::l)



end

module UtilTests =
struct
	let active = false

	let test0 () =
		Util.println (Util.load_file "examples/fa_abc.json")


	let test1 () =
		()

	let runAll =
		if active then (
			Util.header "UtilTests";
			test0 ();
			test1 ()
		)
end


(* --- Set --- *)

module Set = struct
	type 'a t = 'a list
	let empty: 'a = []
	let size (s: 'a list): int = List.length s
	let belongs (v: 'a) (s: 'a t): bool = List.mem v s
	let clear (l: 'a list): 'a t = List.sort_uniq compare l
	let union (s1: 'a t) (s2: 'a t): 'a t = clear (s1 @ s2)
	let add (v: 'a) (s: 'a t): 'a t = clear (v :: s)
	let inter (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun x -> belongs x s2) s1
	let diff (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun x -> not (belongs x s2)) s1
	let subset (s1: 'a t) (s2: 'a t): bool = List.for_all (fun x -> belongs x s2) s1

	let map f s = clear (List.map f s)
	let filter f s = List.filter f s
	let for_all f s = List.for_all f s
	let exists f s = List.exists f s
	let flatMap f s = clear (List.flatten (List.map f s))
	let iter f s = List.iter f s
	let partition f s = let (a, b) = List.partition f s in (clear a, clear b)

	let hasDuplicates (s: 'a list): bool = size s <> size (clear s)
	let validate (l: 'a list) (culprit: string): 'a t =
		if hasDuplicates l
			then Error.error culprit "Repetitions in set" empty
			else clear l
end


(* --- JSon --- *)

module JSon = struct
	type t = Yojson.Basic.t

	type 'r alternatives =
		  JSon of t
		| Text of string
		| File of string
		| Representation of 'r

	let error = Error.error

	let from_string (str: string) =
		Yojson.Basic.from_string str

	let to_string (j: t) =
		Yojson.Basic.pretty_to_string j

	let show (j: t) =
		Util.println (to_string j)

	let from_file filename =
		from_string (Util.load_file filename)

	let from_alternatives alt =
		match alt with
			  JSon j -> j
			| Text str -> from_string str
			| File str -> from_file str
			| Representation r -> `Null

	let get_representation alt =
		match alt with
			  JSon j -> failwith "get_representation"
			| Text str -> failwith "get_representation"
			| File str -> failwith "get_representation"
			| Representation r -> r

	let field_string (j: t) (field: string) =
		match j |> member field with
			  `Null -> error field "Missing field" "#"
			| `String s -> s
			| _ -> error field "Expected string" "#"

	let as_string (j: t) (field: string) =
		match j with
			  `String s -> s
			| _ -> error field "Expected string" "#"

	let field_string_list (j: t) (field: string) =
		match j |> member field with
			  `Null -> error field "Missing field" []
			| `List l -> List.map (fun j -> as_string j field) l
			| _ -> error field "Expected string list" []

	let field_string_set (j: t) (field: string) =
		Set.validate (field_string_list j field) field

	let as_char (j: t) (field: string) =
		match j with
			  `String s when String.length s = 1 -> String.get s 0
			| _ -> error field "Expected char" '#'

	let field_char_list (j: t) (field: string) =
		match j |> member field with
			  `Null -> error field "Missing field" []
			| `List l -> List.map (fun j -> as_char j field) l
			| _ -> error field "Expected char list" []

	let field_char_set (j: t) (field: string) =
		Set.validate (field_char_list j field) field

	let as_string_char_string (j: t) (field: string) =
		match j with
			  `List [a; b; c] -> (as_string a field, as_char b field, as_string c field)
			| _ -> error field "Malformed triple" ("#",'#',"#")

	let field_triples_list (j: t) (field: string) =
		match j |> member field with
		  `List l -> List.map (fun j -> as_string_char_string j field) l
		| _ -> []

	let field_triples_set (j: t) (field: string) =
		Set.validate (field_triples_list j field) field

	let identification (j: t): string * string * string =
		let kind = field_string j "kind" in
		let description = field_string j "description" in
		let name = field_string j "name" in
			(kind, description, name)

	let makeDummyIdentification (k: string): t =
		`Assoc [("kind", `String k);
				("description", `String "_");
				("name", `String "_")]
end

module JSonTests = struct
	let active = false

	let test0 () =
		let str = "{
					name: {
						first: \"aa\",
						last: 22,
						fullname: 33
					},
					age: 33,
					hobbies: [ 44, 55 ]
				}" in
		let json = JSon.from_string str in
			JSon.show json

	let test1 () =
		let json = JSon.from_file "examples/fa_abc.json" in
			JSon.show json

	let runAll =
		if active then (
			Util.header "JSonTests";
			test0 ();
			test1 ()
		)
end


module RegExpSyntax = struct
		(*  Grammar:
				E -> E + E | E E | E* | c | (E) | ()

			Grammar with priorities:
				E -> T | T + E
				T -> F | F T
				F -> A | A*
				A -> P | c
				P -> (E) | ()
		*)
	type t =
		| Plus of t * t
		| Seq of t * t
		| Times of t
		| Symb of char
		| Empty
		| Zero
	;;

	let inputString = ref ""
	let inputStringLength = ref 0
	let inputStringPosition = ref 0

	let skip () =
		inputStringPosition := !inputStringPosition + 1

	let rec curr () =
		if !inputStringPosition >= !inputStringLength then
			' '
		else if String.get !inputString !inputStringPosition = ' ' then
			( skip(); curr() )
		else
			String.get !inputString !inputStringPosition

	let rec parse_exp () =
		let t = parse_term () in
			match curr() with
				| '+' -> skip(); Plus (t, parse_exp ())
				| _ -> t

	and parse_term () =
		let f = parse_factor () in
			match curr() with
				| '+' | ')' | ' ' -> f
				| _ -> Seq (f, parse_term ())

	and parse_factor () =
		let a = parse_atom () in
			match curr() with
				| '*' -> skip(); (Times a)
				| _ -> a

	and parse_atom () =
		match curr() with
			| '(' -> skip(); parse_parentised ()
			| '+' | '*' -> failwith "Invalid use of wildcard\n"
			| ' '  -> failwith "Premature end of expression\n"
			| c -> skip(); (Symb c)

	and parse_parentised () =
		match curr() with
			| ')' -> skip(); Empty
			| _ ->
				let e = parse_exp () in (
		        	match curr() with
		         		| ')' -> skip(); e
		                | _ -> failwith "Right-parenthesis expected\n"
			 	)

	let parse s =
		inputString := s;
		inputStringLength := String.length s;
		inputStringPosition := 0;
		parse_exp ()

	let rec toStringN n re =
		match re with
			| Plus(l, r) ->
					(if n > 0 then "(" else "") ^
					toStringN 0 l ^ "+" ^ toStringN 0 r
					^ (if n > 0 then ")" else "")
			| Seq(l, r) ->
					(if n > 1 then "(" else "") ^
					toStringN 1 l ^ toStringN 1 r
					^ (if n > 1 then ")" else "")
			| Times(r) ->
					toStringN 2 r ^ "*"
			| Symb(c) -> String.make 1 c
			| Empty -> "()"
			| Zero -> ""

	let toString re =
		toStringN 0 re

	let show re =
		Util.println (toString re)
end

module RegExpSyntaxTests = struct
	let active = false

	let test0 () =
		let re = RegExpSyntax.parse "ab+()*" in
			RegExpSyntax.show re

	let test1 () =
		let re = RegExpSyntax.parse "()((a+b)*(cd)*)*" in
			RegExpSyntax.show re

	let runAll =
		if active then (
			Util.header "RegExpSyntaxTests";
			test0 ();
			test1 ()
	)
end


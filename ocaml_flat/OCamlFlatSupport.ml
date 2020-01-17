(** OCaml-Flat - An OCaml library for FLAT concepts
 *
 * Support code for the library
 *
 * @version v 0.3
 *
 * @author Artur Miguel Dias <amd@fct.unl.pt>  [main author]
 * @author Joao Goncalves <jmg.goncalves@campus.fct.unl.pt>
 * @author Rita Macedo <rp.macedo@campus.fct.unl.pt>
 * @author Antonio Ravara <aravara@fct.unl.pt>
 *
 * LICENCE - As of now this is a private project, but later it
 *            will be open-source.
*)

open Yojson.Basic.Util


(* --- Error --- *)

module Error = struct
	type t = string list ref

	let sink: t = ref []

	let err: t ref = ref sink

	let start () =	(* setup a log of errors *)
		let r = ref [] in
			err := r;
			r		(* returns a new error log *)

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

	let printAlphabet (alf:char list) =
		List.iter (fun x -> print_char x; print_string ", ") alf;
		println " "

	let printStates (st:string list) =
		List.iter (fun x -> print_string x; print_string ", ") st;
		println " "

	let printTransition (a:string) (b:char) (c:string) =
		print_string "("; print_string a; print_string ", ";
		print_char b; print_string ", ";
		print_string c; println ")"

	let printWord (w:char list) =
		print_char '\'' ; List.iter (fun c -> print_char c ) w; println "'"

	let printWordList (l:char list list) =
		List.iter printWord l

	let listFromString s = List.init (String.length s) (String.get s)

	let stringFromList l =
		let buf = Buffer.create 16 in
		let () = List.iter (Buffer.add_char buf) l in
			Buffer.contents buf

	let addAll symb s = List.map (fun l -> symb::l) s

	let concatAll w s = List.map (fun l -> w@l) s

	let distrib2 f (a,b) = f a b
	
	let stringToWord s = 
		let n = String.length s in
		let rec iterStr i =
			if i < n then (s.[i])::(iterStr (i+1))
			else []
		in
			iterStr 0	

	let wordToString w = 
		let ws = List.map (fun c -> String.make 1 c) w in
			String.concat "" ws	
		

	let indexOf e l =
		let rec index e l n =
			match l with
				[] -> -1
				|x::xs -> if e = x then n else index e xs (n+1)
		in
		index e l 0

	let rec fixedPoint (f: 'a -> 'a) (x: 'a): 'a =
		let next = f x in
			if x = next then x
			else fixedPoint f next
end

module UtilTests =
struct
	let active = false

	let test0 () =
		Util.println (Util.load_file "fa_abc.json")


	let test1 () =
		let a = Util.wordToString ['e';'r';'t'] in
		let b = Util.wordToString ['4';'5';'y'] in
			print_string a; print_string b

	let runAll =
		if active then (
			Util.header "UtilTests";
			test0 ();
			test1 ()
		)
end


(* --- Set --- *)

module Set : (* regular set *)
sig
	(*type 'a t = 'a list*)
	type 'a t
	val make : 'a list -> 'a t
	val toList : 'a t -> 'a list
	val empty : 'a t
	val size : 'a t -> int
	val belongs : 'a -> 'a t -> bool
	val union : 'a t -> 'a t -> 'a t
	val nth : 'a t -> int -> 'a 
	val add : 'a -> 'a t -> 'a t
	val remove : 'a -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val subset : 'a t -> 'a t -> bool
	val map : ('a -> 'b) -> 'a t -> 'b t
	val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
	val filter : ('a -> bool) -> 'a t -> 'a t
	val for_all : ('a -> bool) -> 'a t -> bool
	val exists : ('a -> bool) -> 'a t -> bool
	val flatten : 'a t t -> 'a t
	val flatMap : ('a -> 'b t) -> 'a t -> 'b t
	val iter : ('a -> unit) -> 'a t -> unit
	val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
	val combinations : 'a t -> 'b t -> ('a * 'b) t
	val star : 'a list t -> int -> 'a list t
	val allDistinct : ('a -> 'b) -> 'a t -> bool
	val hasDuplicates : 'a t -> bool
	val validate : 'a list -> string -> 'a t
	val test: unit -> int list list
  end
= struct
	type 'a t = 'a list
	let make (l: 'a list): 'a t = List.sort_uniq compare l
	let toList (s: 'a t): 'a list = s

	let empty: 'a t = []
	let size (s: 'a t): int = List.length s
	let belongs (v: 'a) (s: 'a t): bool = List.mem v s
	let union (s1: 'a t) (s2: 'a t): 'a t = make (s1 @ s2)
	let nth (s: 'a t) (n: int) : 'a  = List.nth s n
	let add (v: 'a) (s: 'a t): 'a t = make (v :: s)
	let remove (v: 'a) (s: 'a t): 'a t = List.filter (fun x -> x <> v) s
	let inter (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun x -> belongs x s2) s1
	let diff (s1: 'a t) (s2: 'a t): 'a t = List.filter (fun x -> not (belongs x s2)) s1
	let subset (s1: 'a t) (s2: 'a t): bool = List.for_all (fun x -> belongs x s2) s1

	let map f s = make (List.map f s)
	let mapi f s = make (List.mapi f s)
	let filter f s = List.filter f s
	let for_all f s = List.for_all f s
	let exists f s = List.exists f s
	let flatten ss = make (List.flatten ss)
	let flatMap f s = flatten (List.map f s)
	let iter f s = List.iter f s
	let partition f s = let (a, b) = List.partition f s in (a, b)  (* <- already ordered *)
	let combinations (s1: 'a t) (s2: 'b t): ('a * 'b) t =
		flatMap (fun x -> List.map (fun y -> (x,y)) s2) s1  (* <- already ordered *)
	let starOne (s: 'a list t) (n: int) (l:'a list): 'a list t = (* private auxiliary *)
		let z = n - (List.length l) in
		let sel = filter (fun k -> List.length k <= z) s in
			map (fun k -> k @ l) sel
	let star (s: 'a list t) (n: int): 'a list t =
		Util.fixedPoint (fun x -> union x (flatMap (starOne x n) s)) [[]]

	let allDistinct f s = size s = size (map f s)
	let hasDuplicates (s: 'a t): bool = size s <> size (make s)
	let validate (l: 'a list) (culprit: string): 'a t =
		if hasDuplicates l
			then Error.error culprit "Repetitions in set" empty
			else make l

	let test (): int list list =  (* Set.test ();; *)
		toList (star (make[ [1]; [2;3]]) 4)
end

module UPSet : (* unordered pair set *)
  sig
	type 'a t
	val make : ('a * 'a) list -> 'a t
	val toList : 'a t -> ('a * 'a) list
	val empty : 'a t
	val size : 'a t -> int
	val belongs : 'a * 'a -> 'a t -> bool
	val union : 'a t -> 'a t -> 'a t
	val add : 'a * 'a -> 'a t -> 'a t
	val inter : 'a t -> 'a t -> 'a t
	val diff : 'a t -> 'a t -> 'a t
	val subset : 'a t -> 'a t -> bool
	val map : ('a * 'a -> 'b * 'b) -> 'a t -> 'b t
	val filter : ('a * 'a -> bool) -> 'a t -> ('a * 'a) Set.t
	val for_all : ('a * 'a -> bool) -> 'a t -> bool
	val exists : ('a * 'a -> bool) -> 'a t -> bool
	val flatten : 'a t t -> 'a t
	val flatMap : ('a -> 'b t) -> 'a t -> 'b t
	val iter : ('a * 'a -> unit) -> 'a t -> unit
	val partition : ('a * 'a -> bool) -> 'a t -> ('a * 'a) Set.t * ('a * 'a) Set.t
	val combinations : 'a t -> 'b t -> ('a * 'b) t
	val star : 'a list t -> int -> 'a list t
	val allDistinct : ('a * 'a -> 'b) -> 'a t -> bool
	val hasDuplicates : 'a t -> bool
	val validate : ('a * 'a) list -> string -> 'a t
	val test: unit -> (int * int) list
  end
 =
 struct
	type 'a t = ('a*'a) Set.t

	(* invariant: a < b for all pairs (a,b) *)
	let ord (a,b) = if a < b then (a, b)        (* keep *)
					else if a > b  then (b, a)  (* swap *)
					else failwith "UPSet.ord"   (* error *)

	let make (l: ('a*'a) list): 'a t =
		let l1 = List.filter (fun (a,b) -> a <> b) l in
		let l2 = List.map ord l1 in
			Set.make l2
	let toList (s: 'a t): ('a*'a) list = Set.toList s

	let empty: 'a t = Set.empty
	let size (s: 'a t): int = Set.size s
	let belongs (v: 'a*'a) (s: 'a t): bool = Set.belongs (ord v) s
	let union (s1: 'a t) (s2: 'a t): 'a t = Set.union s1 s2
	let add (v: 'a*'a) (s: 'a t): 'a t = Set.add (ord v) s
	let inter (s1: 'a t) (s2: 'a t): 'a t = Set.inter s1 s2
	let diff (s1: 'a t) (s2: 'a t): 'a t = Set.diff s1 s2
	let subset (s1: 'a t) (s2: 'a t): bool = Set.subset s1 s2

	let map f (s: 'a t) = make (Set.toList (Set.map f s))
	let filter f (s: 'a t) = Set.filter f s
	let for_all f (s: 'a t) = Set.for_all f s
	let exists f (s: 'a t) = Set.exists f s
	let flatten (ss: 'a t t) = failwith "UPSet.flatten"
	let flatMap f (s: 'a t) = failwith "UPSet.flatMap"
	let iter f (s: 'a t) = Set.iter f s
	let partition f (s: 'a t) = Set.partition f s
	let combinations (s1: 'a t) (s2: 'b t): ('a * 'b) t = failwith "UPSet.combinations"
	let star (s: 'a list t) (n: int): 'a list t = failwith "UPSet.star"

	let allDistinct f (s: 'a t) = Set.allDistinct f s
	let hasDuplicates (s: 'a t): bool = Set.hasDuplicates s
	let validate (l: ('a*'a) list) (culprit: string): 'a t = failwith "UPSet.validate"
	let test () =  (* UPSet.test ();; *)
		toList (make [(1,1);(1,2);(2,2);(3,2);(3,2);(2,3)])
end


(* --- JSon --- *)

module JSon = struct
	type t = Yojson.Basic.t

	type 'r alternatives =
		| JSon of t
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
			| JSon j -> j
			| Text str -> from_string str
			| File str -> from_file str
			| Representation r -> `Null

	let get_representation alt =
		match alt with
			| JSon j -> failwith "get_representation"
			| Text str -> failwith "get_representation"
			| File str -> failwith "get_representation"
			| Representation r -> r

	let field_string (j: t) (field: string) =
		match j |> member field with
			| `Null -> error field "Missing field" "#"
			| `String s -> s
			| _ -> error field "Expected string" "#"

	let as_string (j: t) (field: string) =
		match j with
			| `String s -> s
			| _ -> error field "Expected string" "#"

	let field_string_list (j: t) (field: string) =
		match j |> member field with
			| `Null -> error field "Missing field" []
			| `List l -> List.map (fun j -> as_string j field) l
			| _ -> error field "Expected string list" []

	let field_string_set (j: t) (field: string) =
		Set.validate (field_string_list j field) field

	let as_char (j: t) (field: string) =
		match j with
			| `String s when String.length s = 1 -> String.get s 0
			| _ -> error field "Expected char" '#'

	let field_char_list (j: t) (field: string) =
		match j |> member field with
			| `Null -> error field "Missing field" []
			| `List l -> List.map (fun j -> as_char j field) l
			| _ -> error field "Expected char list" []

	let field_char_set (j: t) (field: string) =
		Set.validate (field_char_list j field) field

	let as_string_char_string (j: t) (field: string) =
		match j with
			| `List [a; b; c] -> (as_string a field, as_char b field, as_string c field)
			| _ -> error field "Malformed triple" ("#",'#',"#")

	let field_triples_list (j: t) (field: string) =
		match j |> member field with
			| `List l -> List.map (fun j -> as_string_char_string j field) l
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
		let json = JSon.from_file "fa_abc.json" in
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
		| Star of t
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
				| '*' -> skip(); (Star a)
				| _ -> a

	and parse_atom () =
		match curr() with
			| '~' -> skip(); Empty
			| '!' -> skip(); Zero
			| '(' -> skip(); parse_parentised ()
			| '+' | '*' -> failwith "Invalid use of wildcard\n"
			| ' '  -> failwith "Premature end of expression\n"
			| c -> skip(); (Symb c)

	and parse_parentised () =
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
			| Star(r) ->
					toStringN 2 r ^ "*"
			| Symb(c) -> String.make 1 c
			| Empty -> "~"
			| Zero -> "!"

	let toString re =
		toStringN 0 re

	let show re =
		Util.println (toString re)
end

module RegExpSyntaxTests = struct
	let active = false

	let test0 () =
		let re = RegExpSyntax.parse "ab+~*" in
			RegExpSyntax.show re

	let test1 () =
		let re = RegExpSyntax.parse "~((a+b)*(cd)*)*" in
			RegExpSyntax.show re

	let runAll =
		if active then (
			Util.header "RegExpSyntaxTests";
			test0 ();
			test1 ()
	)
end
 
 
module CFGSyntax = struct
	type rule = {
		head: char;
		body: char list
	}
	type rules = rule list

	let inputString = ref ""
	let inputStringLength = ref 0
	let inputStringPosition = ref 0

	let isWhite c =
		List.mem c [' '; '\t']

	let skip () =
		inputStringPosition := !inputStringPosition + 1

	let rec curr () =
		if !inputStringPosition >= !inputStringLength then
			' '
		else if isWhite (String.get !inputString !inputStringPosition) then
			( skip(); curr() )
		else
			String.get !inputString !inputStringPosition

	let rec parse_head () =
		match curr() with
			  ' ' -> failwith "Premature end of expression\n"
			| c -> skip() ; c

	let rec parse_neck () =
		match curr() with
			  ' ' -> failwith "Premature end of expression\n"
			| '-' -> skip();
					if curr() = '>' then skip()
					else failwith "Bad neck\n"
			| _ -> failwith "Bad neck\n"

	let rec parse_body () =
		match curr() with
			  ' ' -> [[]]
			| '|' -> skip(); []::parse_body ()
			| c -> skip();
					match parse_body () with
                         [] -> failwith "never happens"
                       | x::xs -> (c::x)::xs

	let parse_line line =
		if String.trim line = "" then []
		else (
			inputString := line;
			inputStringLength := String.length line;
			inputStringPosition := 0;
			let h = parse_head () in
			let () = parse_neck () in
			let bs = parse_body () in
				List.map (fun b -> {head=h; body=b}) bs
		)

	let parse rs =
		List.flatten (List.map parse_line rs)


	let toString1 r =
		let full = [r.head; ' '; '-'; '>' ; ' '] @ r.body in
			String.concat "" (List.map (String.make 1) full)

	let toString rs =
		String.concat "\n" (List.map toString1 rs)

	let show rs =
		Util.println (toString rs)
end

module CFGSyntaxTests = struct
	let active = false

	let test0 () =
		let cfg = [ "S -> aTb | ~"; "T -> aSb" ] in
		let rules = CFGSyntax.parse cfg in
			CFGSyntax.show rules

	let test1 () =
		let cfg = ["S -> aSb | ~"] in
		let rules = CFGSyntax.parse cfg in
			CFGSyntax.show rules

	let runAll =
		if active then (
			Util.header "CFGSyntaxTests";
			Util.header "test0";
			test0 ();
			Util.header "test1";
			test1 ();
			Util.header ""
	)
end


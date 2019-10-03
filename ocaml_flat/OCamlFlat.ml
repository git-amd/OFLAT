(* OCamlFlat.ml - AMD/2019 *)

open OCamlFlatSupport

(* --- Configuration --- *)

module Configuration = struct
	let automaticDiagnostics = ref true

	let diagnosticsOn () = !automaticDiagnostics
end


(* --- Basic types --- *)

type 'a set = 'a Set.t
type json = JSon.t
type error = Error.t

type symbol = char
type symbols = symbol set
let epsilon = '~'  (* used for representing the empty transitions *)

type word = symbol list
type words = word set

type state = string
type states = state set


(* --- Entity --- *)

module rec Entity : sig
	class virtual entity :
	  'r JSon.alternatives -> string ->
		  object
			method kind : string
			method description : string
			method name : string
			method errors : error
			method handleErrors : unit
			method virtual validate : unit
			method virtual toJSon : json
		  end
	end
=
struct
	class virtual entity (arg: 'r JSon.alternatives) (expectedKind: string) =
		let errors = Error.start () in
		let r = JSon.from_alternatives arg in
		let j = if r <> `Null then r else JSon.makeDummyIdentification(expectedKind) in
		let (kind, description, name) = JSon.identification j in
		object(self)
			method kind: string = kind
			method description: string = description
			method name: string = name
			method errors = errors
			method virtual validate: unit
			method virtual toJSon: json
			method handleErrors = (
				if self#kind <> expectedKind then
					Error.error self#kind "Wrong kind" ();
				self#validate;
				if Configuration.diagnosticsOn () then
					Error.show expectedKind self#name;
				Error.stop ()
			)
	end
end


(* --- Model --- *)

and (*module*) Model : sig
	val loadModel : string -> Model.model
	class virtual model :
	  'r JSon.alternatives -> string ->
		  object
			method kind : string
			method description : string
			method name : string
			method errors : error
			method handleErrors : unit
			method virtual validate : unit
			method virtual toJSon: json

			method virtual accept : word -> bool
			method virtual generate : int -> words
			method virtual tracing : unit
		  end
end
 =
struct
	let loadModel (filename: string): Model.model =   (* will load any model *)
		let j = JSon.from_file filename in
			let kind = JSon.field_string j "kind" in
				if FiniteAutomaton.modelDesignation () = kind then
					(new FiniteAutomaton.model (JSon j) :> Model.model)
				else if RegularExpression.modelDesignation () = kind then
					(new RegularExpression.model (JSon j) :> Model.model)
				else
					(new FiniteAutomaton.model (JSon j) :> Model.model)

	class virtual model (arg: 'r JSon.alternatives) (expectedKind: string) =
		object(self) inherit Entity.entity arg expectedKind
			method virtual validate: unit
			method virtual accept: word -> bool
			method virtual generate: int -> words
			method virtual tracing: unit
	end

end


(* --- Finite Automaton --- *)

and (*module*) FiniteAutomaton : sig
	type transition = state * symbol * state
	type transitions = transition set
	type t = {
		alphabet : symbols;
		allStates : states;
		initialState : state;
		transitions : transitions;
		acceptStates : states;
	}
	val modelDesignation: unit -> string (* a funtion required for module recursive call *)
	class model :
	  t JSon.alternatives ->
		  object
			method kind : string
			method description : string
			method name : string
			method errors : error
			method handleErrors : unit
			method validate : unit
			method toJSon: json

			method accept : word -> bool
			method accept2 : word -> bool
			method generate : int -> words
			method tracing : unit
			method reachable : state -> states
			method productive : states

			method isDeterministic : bool
			method toDeterministic : unit
			method minimize : unit
			method toRegularExpression : RegularExpression.model

			val representation : t
		  end
end
 =
struct
	type transition =
		state	(* state *)
	  * symbol	(* consumed input symbol *)
	  * state	(* next state *)

	type transitions = transition set

	type t = {
		alphabet: symbols;			(* Alphabet *)
		allStates: states;			(* states *)
		initialState: state;		(* Initial state *)
		transitions: transitions;	(* Transition relation *)
		acceptStates: states		(* Accept states *)
	}

	let modelDesignation () = "finite automaton"

	class model (arg: t JSon.alternatives) =
		object(self) inherit Model.model arg (modelDesignation ())

			val representation: t =
				let j = JSon.from_alternatives arg in
					if j = `Null then
						JSon.get_representation arg
					else
						let alphabet = JSon.field_char_set j "alphabet" in
						let allStates = JSon.field_string_set j "states" in
						let initialState = JSon.field_string j "initialState" in
						let transitions = JSon.field_triples_set j "transitions" in
						let acceptStates = JSon.field_string_set j "acceptStates" in
							{	alphabet = alphabet;
								allStates = allStates;
								initialState = initialState;
								transitions = transitions;
								acceptStates = acceptStates
							}

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method toJSon: json =
				let rep = representation in
				`Assoc [
					("kind", `String self#kind);
					("description", `String self#description);
					("name", `String self#name);
					("alphabet", `List (List.map (fun c -> `String (String.make 1 c)) rep.alphabet));
					("states", `List (List.map (fun s -> `String s) rep.allStates));
					("initialState", `String rep.initialState);
					("transitions", `List (List.map (fun (a,b,c) ->
						`List [`String a; `String (String.make 1 b); `String c]) rep.transitions));
					("acceptStates", `List (List.map (fun s -> `String s) rep.acceptStates))
				]


			method validate: unit = (
            			    (* is initial state a valid state *)
            				if not (Set.belongs representation.initialState representation.allStates) then
            					Error.error representation.initialState
            						"initial state does not belong to the set of all states" ()
            				;
            				(* are all accepted states valid *)
            				if not (Set.subset representation.acceptStates representation.allStates) then
            					Error.error self#name
            					    "not all accepted states belong to the set of all states" ()
            				;
            				(* are all transitions valid *)
            				let fromSt = Set.map ( fun (a, _, _) -> a ) representation.transitions in
            				let sy = Set.map ( fun (_, b, _) -> b ) representation.transitions in
            				let toSt = Set.map ( fun (_, _, c) -> c ) representation.transitions in
            				let alpha = representation.alphabet@[epsilon] in
            				let cond = (Set.subset fromSt representation.allStates) &&
            				(Set.subset sy alpha) && (Set.subset toSt representation.allStates) in
            				if not cond then
            					    Error.error self#name
            					        "not all transitions are valid" ()
            			    )


			method accept2 (w: word): bool =
			    let nextStates sy st t =
                	let n = List.filter (fun (a,b,c) -> st = a && sy = b  ) t in
                		List.map ( fun (_,_,d) -> d ) n in
                let rec acc cf t sta =
                    match cf with
                        [] -> false
                        |(st,[])::ls -> List.mem st sta || acc ls t sta
                        |(st,x::xs)::ls ->
                            let n = nextStates x st t in
                            let cfn = List.map (fun c -> (c,xs)) n in
                            let n2 = nextStates epsilon st t in
                            let cfn2 = List.map (fun c -> (c,x::xs)) n2 in
                                acc (ls@cfn@cfn2) t sta in
                acc [(representation.initialState,w)] representation.transitions representation.acceptStates
       

            method accept (w: word): bool =
			
                let nextEpsilon1 st t =
                    let n = Set.filter (fun (a,b,c) -> st = a && b = epsilon) t in
                        Set.map ( fun (_,_,d) -> d ) n in			
				
				let rec nextEpsilons currsts t = 
					let ns = Set.flatMap (fun nst -> nextEpsilon1 nst t) currsts in
						if (Set.subset ns currsts) then ns else nextEpsilons (Set.union currsts ns) t in
						
                let nextStates st sy t =
                    let n = Set.filter (fun (a,b,c) -> st = a && sy = b) t in
                        Set.map ( fun (_,_,d) -> d ) n in
						
                let transition sts sy t =
                    let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
                        Set.union nsts (nextEpsilons nsts t) in
						
                let rec accept2X sts w t =
                    match w with
                        [] -> (Set.inter sts representation.acceptStates) <> []
                        |x::xs -> accept2X (transition sts x t) xs t in
						
                let i = Set.add representation.initialState
                            (nextEpsilons [representation.initialState] representation.transitions) in
                    accept2X i w representation.transitions


			method generate (length: int): words =
			    let es = representation.acceptStates in
			    let t = representation.transitions in
			    let rec gen n is =
                	if n = 0 then if Set.belongs is es then [[]] else []
                	else let (x,_) = Set.partition (fun (a,_,_) -> a = is) t in
                        Set.flatMap (fun (_,b,c) -> if b = epsilon then gen (n-1) c else Util.addAll b (gen (n-1) c )) x
                in
                gen length representation.initialState


            (*print configs during accept*)
			method tracing: unit = ()

            (*all states reachable from s*)
            method reachable (s:state): states =
                let proj3 t = Set.map ( fun (_,_,d) -> d ) t in
                let nextStates s t = Set.flatMap ( fun x -> proj3 (Set.filter (fun (a,_,_) -> x = a) t) ) s in
                let remain s t = Set.filter (fun (a,_,_) -> not (Set.belongs a s)) t in
                let rec reach cs s t = if cs = s then [] else Set.union s ( reach s (nextStates s t) (remain s t) ) in
                reach [] [s] representation.transitions


            method productive: states =
                Set.filter (fun x -> Set.exists (fun y -> Set.belongs y representation.acceptStates )
                (self#reachable x) ) representation.allStates


			method isDeterministic: bool =
			    let ts = representation.transitions in
                let eps = Set.exists (fun (_,y,_) -> y = epsilon ) ts in
                let ndet = Set.map ( fun (a,b,c) -> Set.exists (fun (x,y,z) -> a = x && b = y && c <> z ) ts ) ts in
                let n = Set.exists (fun x -> x = true) ndet in
                eps || n

            method toDeterministic: unit = ()


			method minimize = ()

			(*
			let usfSts = Set.inter self#productive (self#reachable representation.initialState) in
                        let usfTrs = Set.filter (fun (a,_,c) -> Set.belongs a usfSts &&
                        Set.belongs c usfSts) representation.transitions in
                        let usfAlf = Set.map ( fun (_,a,_) -> a ) usfTrs in
                        let (inF, notF) = Set.partition (fun x -> Set.belongs x representation.acceptStates) usfSts in
                        let distI1 = Set.flatMap ( fun a -> Set.map ( fun b -> (a,b) ) inF ) notF in

                        let tMtrx = Array.make_matrix (Set.size usfSts) (Set.size usfAlf) [] in
                        let r = Set.iteri (fun ia a -> Set.iteri (fun ib b -> tMtrx.(ia).(ib) <-
                        Set.filter (fun (x,y,_) -> a = x && b = y) usfTrs) usfAlf) usfSts in
                        let combine p = fun (a,b) -> Set.flatMap (fun x -> Set.map ( fun y -> (x,y) ) b) a in
                        let distI2 = List.mapi (fun ia alf -> combine (Set.partition
                        (fun st -> !(tMtrx.(Util.indexOf st usfSts).(ia) = [])) usfSts) ) usfAlf in
                        (* Set.exists (fun (x,y,_) -> x = st && y = alf) usfTrs *)

                        let distI = Set.union distI1 distI2 in
                        let stsTimesSts = Set.flatMap (fun a -> Set.map (fun b -> (a,b)) usfSts) usfSts

                        let findR sts = Set.flatMap (fun (a,b) ->  ) st in

                        let rec aped p q = if (q = [] || (Set.union p q) = stsTimesSts) then Set.union p q
                            else aped (Set.union p q) (findR q) in
                        let dist = aped distI (findR distI)
			*)



			method toRegularExpression =
				new RegularExpression.model (Representation (RegExpSyntax.parse "ab+c*"))
		end

end

and (*module*) FiniteAutomatonTests : sig
end
 =
struct
	let active = false

	let test0 () =
		let m = Model.loadModel "examples/fa_abc.json" in
			let j = m#toJSon in
				JSon.show j

	let test1 () =
		let fa = new FiniteAutomaton.model (File "examples/fa_abcd.json") in
			let j = fa#toJSon in
				JSon.show j

	let test2 () =
  		let fa = new FiniteAutomaton.model (File "examples/fa_abc.json") in
  		    Util.println "generated words:"; Util.printWordList (fa#generate 1 );
  		    Util.println "generated words:"; Util.printWordList (fa#generate 2 );
  		    Util.println "generated words:"; Util.printWordList (fa#generate 3 )

    let test3 () =
      		let fa = new FiniteAutomaton.model (File "examples/fa_abc.json") in
      		    Util.println "reachable states:"; Util.printStates (fa#reachable "START" ); Util.println " "

    let test4 () =
          		let fa = new FiniteAutomaton.model (File "examples/fa_abc.json") in
          		    Util.println "productive states:"; Util.printStates (fa#productive ); Util.println " "

    let test5 () =
          		let fa = new FiniteAutomaton.model (File "examples/fa_abc.json") in
          		    if fa#isDeterministic then
          		        Util.println "automata is non-deterministic" else Util.println "automata is deterministic"


    let test6 () =
          		let fa = new FiniteAutomaton.model (File "examples/fa_abc.json") in
          		   if fa#accept ['b'] then Util.println "word was accepted" else Util.println "word was not accepted" ;
          		   if fa#accept ['b';'b'] then Util.println "word was accepted" else Util.println "word was not accepted";
          		   if fa#accept ['a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          		   if fa#accept ['a';'d'] then Util.println "word was accepted" else Util.println "word was not accepted";
          		   if fa#accept ['a';'d';'a';'b';'c'] then Util.println "word was accepted" else Util.println "word was not accepted";
          		   if fa#accept ['c'] then Util.println "word was accepted" else Util.println "word was not accepted"





	let runAll =
		if active then (
			Util.header "FiniteAutomatonTests";
			test6 ()
		)
end


(* --- Regular Expression --- *)

and (*module*) RegularExpression : sig
	type t = RegExpSyntax.t
	val modelDesignation: unit -> string
	class model :
	  t JSon.alternatives ->
		  object
			method kind : string
			method description : string
			method name : string
			method errors : error
			method handleErrors : unit
			method validate : unit
			method toJSon: json

			method accept : word -> bool
			method generate : int -> words
			method tracing : unit

			method toFiniteAutomaton : unit

			val representation : t
		  end
end
 =
struct
	type t = RegExpSyntax.t

	let modelDesignation () = "regular expression"

	class model (arg: t JSon.alternatives) =
		object(self) inherit Model.model arg (modelDesignation ())

			val representation: t =
				let j = JSon.from_alternatives arg in
					if j = `Null then
						JSon.get_representation arg
					else
						let re = JSon.field_string j "re" in
							RegExpSyntax.parse re

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method toJSon: json =
				let rep = representation in
				`Assoc [
					("kind", `String self#kind);
					("description", `String self#description);
					("name", `String self#name);
					("re", `String (RegExpSyntax.toString rep));
				]

			method validate = (
			)

			method accept (w: word): bool = true
			method generate (length: int): words = []
			method tracing: unit = ()

			method toFiniteAutomaton = ()

		end

end

and (*module*) RegularExpressionTests: sig
end
=
struct
	let active = false

	let test0 () =
		let m = Model.loadModel "examples/re_abc.json" in
			let j = m#toJSon in
				JSon.show j

	let test1 () =
		let re = new RegularExpression.model (File "examples/re_abc.json") in
			let j = re#toJSon in
				JSon.show j

	let test2 () =
		let fa = new FiniteAutomaton.model (File "examples/fa_abc.json") in
		let r = fa#toRegularExpression in
		let j = r#toJSon in
			JSon.show j

	let runAll =
		if active then (
			Util.header "RegularExpressionTests";
			test0 ();
			test1 ();
			test2 ()
		)
end












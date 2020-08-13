(** OCaml-Flat - An OCaml library for FLAT concepts  
 *
 * @version v 0.2
 *
 * @author Joao Goncalves <jmg.goncalves@campus.fct.unl.pt>  [main author]
 * @author Rita Macedo <rp.macedo@campus.fct.unl.pt>  
 * @author Artur Miguel Dias <amd@fct.unl.pt>
 * @author Antonio Ravara <aravara@fct.unl.pt>
 *
 * LICENCE - As of now this is a private project, but later it
 *            will be open-source.
*)


open OCamlFlatSupport  

(*#use "OCamlFlatSupport.ml";;*)
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
type variable = char
type variables = char set

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
			method errors : string list
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
			method errors : string list = !errors
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
			method errors : string list
			method handleErrors : unit
			method virtual validate : unit
			method virtual toJSon: json

			method virtual accept : word -> bool
			method virtual generate : int -> words
			method virtual tracing : unit
			method checkExercise : Exercise.exercise -> bool 
			method checkExerciseFailures : Exercise.exercise -> (words * words) 
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
				else if ContextFreeGrammar.modelDesignation () = kind then
					(new ContextFreeGrammar.model (JSon j) :> Model.model)
				else
					(new FiniteAutomaton.model (JSon j) :> Model.model)

	class virtual model (arg: 'r JSon.alternatives) (expectedKind: string) =
		object(self) inherit Entity.entity arg expectedKind
			method virtual validate: unit
			method virtual accept: word -> bool
			method virtual generate: int -> words
			method virtual tracing: unit
			method checkExercise (exercise:Exercise.exercise) = 
						Set.for_all (fun w -> self#accept w) exercise#representation.inside
					 && Set.for_all (fun w -> not (self#accept w)) exercise#representation.outside
					 
			method checkExerciseFailures (exercise:Exercise.exercise) = 
				(Set.filter (fun w -> not (self#accept w)) exercise#representation.inside,
					Set.filter (fun w -> self#accept w) exercise#representation.outside)

	end

end


(* --- Exercise --- *)

and (*module*) Exercise : sig

	type t = {
		problem : string;
		inside : words;
		outside : words
	}		
	
	class exercise :	
		
	  t JSon.alternatives -> 
		  object
			method kind : string
			method description : string
			method name : string
			method errors : string list
			method handleErrors : unit
			method validate : unit
			method toJSon: json
			
			method tracing : unit
			method representation : t
		end
	
end
 =
struct
					
	type t = {
		problem : string;
		inside : words;
		outside : words
	}				
		
	class exercise (arg: 'r JSon.alternatives ) =
		object(self) inherit Entity.entity arg "enumeration"
		
			val representation: t =
				let j = JSon.from_alternatives arg in
					if j = `Null then
						JSon.get_representation arg
					else
						let problem = JSon.field_string j "problem" in
						let inside = JSon.field_string_set j "inside" in
						let outside = JSon.field_string_set j "outside" in
						{
							problem = problem;
							inside = Set.map (fun s -> Util.stringToWord s) inside;
							outside = Set.map (fun s -> Util.stringToWord s) outside
						}
			

		initializer self#handleErrors	(* placement is crucial - after representation *)	
					
		method representation =
				representation 
		method validate = ()
		method toJSon: json =
			let rep = representation in
				`Assoc [
					("kind", `String self#kind);
					("description", `String self#description);
					("name", `String self#name);
					("problem", `String rep.problem );
					("inside", `List (List.map (fun w -> `String (Util.wordToString w)) (Set.toList rep.inside)));
					("outside", `List (List.map (fun w -> `String (Util.wordToString w)) (Set.toList rep.outside)))
				]
		method tracing = ()
	end
end

and (*module*) ExerciseTests : sig
end
 =
struct

let active = false

	let test0 () =
		let e = new Exercise.exercise (File "test enums/enum_astar.json") in
			let j = e#toJSon in
				JSon.show j
				
	let runAll =
		if active then (
			Util.header "EnumerationTests";
			test0 ()
		)
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
			method errors : string list
			method handleErrors : unit
			method validate : unit
			method toJSon: json
			
			method tracing : unit
			
			method acceptBreadthFirst: word -> bool
			method accept : word -> bool
			method acceptWithTracing : word -> unit
			method generate : int -> words
			method generateUntil : int -> words
			method reachable : state -> states
			method productive : states
			method getUsefulStates : states
			method getUselessStates : states
			method cleanUselessStates: FiniteAutomaton.model	
			method areAllStatesUseful: bool
			
			method toDeterministic : FiniteAutomaton.model	
			method isDeterministic : bool					
			method equivalencePartition: states set
			method minimize : FiniteAutomaton.model
			method isMinimized : bool
			
			method toRegularExpression : RegularExpression.model
			method toRegularGrammar : ContextFreeGrammar.model

			method representation : t
			method checkExercise : Exercise.exercise -> bool 
			method checkExerciseFailures : Exercise.exercise -> (words * words) 
		  end
end
 =
struct
	open ContextFreeGrammar

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
	
	(*------Auxiliary functions---------*)
	
	(* get starting state, symbol, and/or end state of all transitions in set  *)
	let transitionGet1 trns = Set.map ( fun (a,_,_) -> a ) trns
	let transitionGet2 trns = Set.map ( fun (_,b,_) -> b ) trns
	let transitionGet3 trns = Set.map ( fun (_,_,c) -> c ) trns
	let transitionGet23 trns = Set.map (fun(_,b,c) -> (b,c)) trns 
	
	(* fuse all states into a new state  *)
	let fuseStates sts = String.concat "_" sts 
	
	
	(* checks if set ts has at least one transition from state st through symbol sy *)
	let hasTrans st sy ts = Set.exists (fun (x,y,_) -> x = st && y = sy) ts 
	
	(* returns the set of state st and all its states reachable by an epsilon transition *)
	let nextEpsilon1 st ts =
        let trns = Set.filter (fun (a,b,c) -> st = a && b = epsilon) ts in
        let nextStates = transitionGet3 trns in	
			Set.add st nextStates 
	
	(* returns the set of states sts and all states reachable from sts through epsilon transitions  *)
	let rec closeEmpty sts t = 
		let ns = Set.flatMap (fun st -> nextEpsilon1 st t) sts in
			if (Set.subset ns sts) then ns else closeEmpty (Set.union sts ns) t 
	
	(* returns states reachable from st through symbol sy *)
	let nextStates st sy t =			
		let n = Set.filter (fun (a,b,c) -> st = a && sy = b) t in
			transitionGet3 n 
	
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

			method representation =
				representation 
			
			method toJSon: json =
				let rep = representation in
				`Assoc [
					("kind", `String self#kind);
					("description", `String self#description);
					("name", `String self#name);
					("alphabet", `List (List.map (fun c -> `String (Util.charToString c)) (Set.toList rep.alphabet)));
					("states", `List (List.map (fun s -> `String s) (Set.toList rep.allStates)));
					("initialState", `String rep.initialState);
					("transitions", `List (List.map (fun (a,b,c) ->
						`List [`String a; `String (Util.charToString b); `String c]) (Set.toList rep.transitions)));
					("acceptStates", `List (List.map (fun s -> `String s) (Set.toList rep.acceptStates)))
				]

				
			
				
			(**
			* This method verifies if the automaton is valid. 
			* An automaton is considered valid if its initial and acceptance states belong to the set of all its states
			* and if all its transitions have states and symbols belonging to the set of all its states and its alphabet respectively.
			* 
			* Desc: If the automaton is invalid, the cause could derive from any combination of the following
			* three options: either the initial state, one of the acceptance states, or one of the transitions does not follow the 
			* previously discussed predicate. This method will print to the console stating which combination of these options caused  
			* the automaton to be invalid
			*)
			method validate: unit = (
			
				(* does initial state belong to the set of all states *)
				let validInitSt = Set.belongs representation.initialState representation.allStates in
				
				
				(* are all accepted states members of all states *)
				let validAccSts = Set.subset representation.acceptStates representation.allStates in				
					
				let fromSt = transitionGet1 representation.transitions in
            	let sy = transitionGet2 representation.transitions in
            	let toSt = transitionGet3 representation.transitions in
            	let alpha = Set.add epsilon representation.alphabet  in
				
				(* do all transitions have states belonging to all states and symbols belonging to the alphabet *)
            	let validTrns = (Set.subset fromSt representation.allStates) &&
            	(Set.subset sy alpha) && (Set.subset toSt representation.allStates) in
				
            	
            	if not validInitSt then
            		Error.error representation.initialState
            			"initial state does not belong to the set of all states" ()
            	;
            	
            	if not validAccSts then
            		Error.error self#name
            			"not all accepted states belong to the set of all states" ()
            	;
            	
            	if not validTrns then
            		Error.error self#name
            			"not all transitions are valid" ()
            	)

							
			method tracing : unit = ()

			
			
			(** 
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be tested for acceptance 
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			* Desc: Checks if the automaton accepts word w using configurations (that is, pairs formed by a state and 
			* a remaining word) and a breadth-first approach as to deal with potential non-termination 
			*)
			method acceptBreadthFirst(w: word): bool = false
			(*			    
                let rec acc cf t sta =
                    match cf with
                        [] -> false
                        |(st,[])::ls -> 
							let accepts = (Set.inter (closeEmpty (Set.make [st]) t) sta) <> Set.empty in
								accepts || acc ls t sta
                        |(st,x::xs)::ls ->
                            let n = nextStates st x t in
                            let cfn = Set.map (fun c -> (c,xs)) n in
                            let n2 = nextStates st epsilon t in
                            let cfn2 = Set.map (fun c -> (c,x::xs)) n2 in
                                acc (Set.flatten (Set.make [ls;cfn;cfn2])) t sta in
                acc (Set.make [(representation.initialState,w)]) representation.transitions representation.acceptStates
			*)

			(** 
			* This method verifies if the given word is accepted by the automaton
			*
			* @param w:word -> word to be accepted 
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			* Desc: Checks if the automaton accepts word w using functions over sets of states 
			*)
            method accept (w: word): bool =
			                						
                let transition sts sy t =
                    let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
                        Set.union nsts (closeEmpty nsts t) in
						
                let rec acceptX sts w t =
                    match w with
                        [] -> (Set.inter sts representation.acceptStates) <> Set.empty
                        |x::xs -> let nextSts = transition sts x t in
							nextSts <> Set.empty && acceptX nextSts xs t in
						
                let i = closeEmpty (Set.make [representation.initialState]) representation.transitions in
                    acceptX i w representation.transitions
			
			
			
			method acceptWithTracing (w:word): unit = 
			
			
				let transition sts sy t =
                    let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
                        Set.union nsts (closeEmpty nsts t) in
						
                let rec acceptX sts w t =
                    match w with
                        [] -> [(w,sts)]
                        |x::xs -> let nextSts = transition sts x t in
									let res = acceptX nextSts xs t in
										(w,sts)::res
				in				
						
                let i = closeEmpty (Set.make [representation.initialState]) representation.transitions in
				
				let res = acceptX i w representation.transitions in
				
				let printRes w sts = 
					print_string "('";
					print_string (Util.wordToString w);
					print_string "',[";
					Set.iter (fun st -> print_string st; print_string ";") sts;
					print_string "])";
					
				in
				
				List.iter (fun (w,sts) -> printRes w sts; print_string ";";) res; Util.println ""
				 
				 
			
			
			(** 
			* This method generates all words of the given size which are accepted by the automaton
			*
			* Precondition -> length >= 0
			*
			* @param length:int -> size of all words to be generated
			*
			* @returns words -> the set of all words with size length
			*)
			method generate (length: int): words =
			
				(* adds symbol to the left of all words *)
				let addSyToRWords symb ws = Set.map (fun l -> symb::l) ws in
				
				let hasAcceptState sts accSts = Set.exists (fun st -> Set.belongs st accSts) sts in
				let nxtNonEmptyTrns st ts = Set.filter (fun (a,b,_) -> a = st && b <> epsilon) ts in
			
				let rec gen n state transitions accSts =	
				
					let clsEmpty = (closeEmpty (Set.make [state]) transitions) in					
					if n = 0 then 
						if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty 
					
					else						
						let trnsSet = Set.flatMap (fun st -> nxtNonEmptyTrns st transitions ) clsEmpty in
						
						let rwords st1 l1 = gen (l1-1) st1 transitions accSts in
						let genX sy st l = addSyToRWords sy (rwords st l) in
						
							   Set.flatMap (fun (_,sy,st) -> genX sy st n) trnsSet 
				in
				gen length representation.initialState representation.transitions representation.acceptStates
            
			
			(** 
			* This method generates all words up to a given size which are accepted by the automaton
			*
			* Precondition -> length >= 0
			*
			* @param length:int -> maximum size of all words to be generated
			*
			* @returns words -> the set of all words with size length or less
			*)
			method generateUntil (length: int): words =
			
				(* adds symbol to the left of all words *)
				let addSyToRWords symb ws = Set.map (fun l -> symb::l) ws in
			
				let hasAcceptState sts accSts = Set.exists (fun st -> Set.belongs st accSts) sts in
				let nxtNonEmptyTrns st ts = Set.filter (fun (a,b,_) -> a = st && b <> epsilon) ts in
			
				let rec gen n state transitions accSts =	
				
					let clsEmpty = (closeEmpty (Set.make [state]) transitions) in					
					if n = 0 then 
						if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty
					
					else						
						let trnsSet = Set.flatMap (fun st -> nxtNonEmptyTrns st transitions ) clsEmpty in
						let genX sy st l = addSyToRWords sy (gen (l-1) st transitions accSts) in
						let lenOneOrMore = Set.flatMap (fun (_,sy,st) -> genX sy st n) trnsSet in
						let lenZero = if hasAcceptState clsEmpty accSts then Set.make [[]] else Set.empty in
							Set.union lenOneOrMore lenZero
				in
				gen length representation.initialState representation.transitions representation.acceptStates
			

            (** 
			* This method generates all states that are reachable from the given state. A state is reachable from s if there 
			* exists a word that starting on s will lead to that state 
			* 
			* @param s:state -> the given state 
			*
			* @returns states -> the set of all states reachable from s. 
			*)
            method reachable (s:state): states =
			
				let neighbourSts st t = transitionGet3 (Set.filter (fun (a,_,_) -> a = st) t) in
                let nextStates sts t = Set.flatMap (fun st -> neighbourSts st t) sts in
                let remain s t = Set.filter (fun (a,_,_) -> not (Set.belongs a s)) t in
				
                let rec reach visited s t = if visited = s then Set.empty else Set.union s ( reach s (nextStates s t) (remain s t) ) in
					reach Set.empty (Set.make [s]) representation.transitions
					
					

			(** 
			* This method generates all productive states. A state is productive if there exists a word that will lead said state 
			* to an acceptance state 
			* 
			* @returns states -> the set of all productive states
			*
			* Desc: For each state of the automaton, this method applies the reachable method and checks if any of the resulting
			* states is an acceptance state, and if it is then that state will belong to the resulting set of productive states
			*)
            method productive: states =
			
				let reachsAccSt st = Set.exists (fun s -> Set.belongs s representation.acceptStates ) (self#reachable st) in
					Set.filter (fun st -> reachsAccSt st) representation.allStates

			(** 
			* This method generates the set of all useful states 
			*
			* @returns states -> the set of all useful states
			*)
			method getUsefulStates: states =
				Set.inter self#productive (self#reachable representation.initialState)
				
				
			(** 
			* This method generates the set of all non useful states 
			*
			* @returns states -> the set of all non useful states
			*)
			method getUselessStates: states =
				Set.diff representation.allStates self#getUsefulStates
				
			
			(** 
			* This method creates the equivalent automaton where all states are useful
			*
			* @returns FiniteAutomaton.model -> the new equivalent automaton where all states are useful
			*
			* Desc: The new automaton is created by eliminating from the original automaton all its non useful states, all transitions 
			* that have a non useful state, and all symbols of the alphabet that only appear in said transitions
			*)
			method cleanUselessStates: FiniteAutomaton.model =
			
				let usfSts = self#getUsefulStates in
                let usfTrs = Set.filter (fun (a,_,c) -> Set.belongs a usfSts &&
														Set.belongs c usfSts) 
								representation.transitions in
					
				let alf = transitionGet2 usfTrs in
				let usfAlf = Set.diff alf (Set.make [epsilon]) in
				let newAccSts = Set.inter representation.acceptStates usfSts in
				let usfSts = Set.add representation.initialState usfSts in
				
				new FiniteAutomaton.model (Representation {alphabet = usfAlf; allStates = usfSts; 
					initialState = representation.initialState;	transitions = usfTrs; acceptStates = newAccSts} )
					
				
			
			(** 
			* This method verifies if all the automaton's states are useful 
			*
			* @returns bool -> true if all states of the automaton are useful, false otherwise
			*)
			method areAllStatesUseful: bool = 
			
				let usfSts = self#getUsefulStates in
					Set.size representation.allStates = Set.size usfSts
				
			
			(** 
			* This method converts the non-deterministic automaton into its deterministic equivalent 
			*
			* @returns FiniteAutomaton.model -> the new deterministic automaton
			*
			* Desc: If the automaton to determinize is already deterministic,
			* the resulting automaton will be equal to the original
			*)
			method toDeterministic: FiniteAutomaton.model = 
			
				let move sts sy ts = Set.flatMap (fun st -> nextStates st sy ts ) sts in	
				
				(* generates the set of states reachable from the given state set though the given symbol *)
				let newR oneR sy ts = 
					let nxtSts = move oneR sy ts in
					let clsempty = closeEmpty nxtSts ts in
					Set.union nxtSts clsempty in
					
				(* creates all transitions (given state set, a given symbol, states reachable from set through given symbol) *)
				let rToTs r = 
					let nxtTrans = Set.map (fun sy -> (r,sy,newR r sy representation.transitions)) representation.alphabet in
						Set.filter (fun (_,_,z) -> not (z = Set.empty)) nxtTrans in
					
				(* applies previous function to all state sets until no new set is generated *)
				let rec rsToTs stsD rD trnsD alph = 
					let nxtTs = Set.flatMap (fun stSet -> rToTs stSet ) rD in
					let nxtRs = Set.map (fun (_,_,z) -> z) nxtTs in
					let newRs = Set.filter (fun r -> not (Set.belongs r stsD)) nxtRs in
					if newRs = Set.empty then (Set.union trnsD nxtTs) else 
						rsToTs (Set.union newRs stsD) newRs (Set.union trnsD nxtTs) alph  in	
				
				
				let r1 = closeEmpty (Set.make [representation.initialState]) representation.transitions in
				
				(* all transitions of the new deterministic automaton *)				
				let trnsD = rsToTs (Set.make [r1]) (Set.make [r1]) Set.empty representation.alphabet in
								
				let tds = Set.map (fun (a,b,c) -> (fuseStates (Set.toList a), b, fuseStates (Set.toList c))) trnsD in
						
				let newInitialState = fuseStates (Set.toList r1) in
				
				let stSet1 = Set.map (fun (a,_,_) -> a) trnsD in
				let stSet2 = Set.map (fun (_,_,c) -> c) trnsD in
				let stSet = Set.union stSet1 stSet2 in
				
				let isAccepState st = Set.belongs st representation.acceptStates in
				let hasAnAccepSt set = Set.exists (fun st -> isAccepState st ) set in
				let newAccStsSet = Set.filter (fun set -> hasAnAccepSt set) stSet in
				
				let newAllSts = Set.map (fun set -> fuseStates (Set.toList set)) stSet in
				let newAccSts = Set.map (fun set -> fuseStates (Set.toList set)) newAccStsSet in
				
								
				new FiniteAutomaton.model (Representation {alphabet = representation.alphabet; 
					allStates = newAllSts; initialState = newInitialState;	transitions = tds; acceptStates = newAccSts} )
				
			(** 
			* This method verifies if the automaton is deterministic 
			*
			* @returns bool -> true if automaton is deterministic, false otherwise
			*
			* Desc: For each state s, this method checks if there exists 2 or more transitions with the same symbol from any  
			* state belonging to closeempty of s, independently of the state which said transitions will lead to. 
			* If there is no state for which this property is true, then the automaton is deterministic 
			*)
			method isDeterministic: bool =
			
				let trnsFromSt st ts = Set.filter (fun (st1,sy,_) -> st1 = st && sy <> epsilon) ts in
			
				let isStDeter st ts =
					let allSts = closeEmpty (Set.make [st]) ts in
					let allTs = Set.flatMap (fun st -> trnsFromSt st ts) allSts in
					let sys = transitionGet2 allTs in
						Set.size allTs = Set.size sys in
						
				let hasNondeterSt = Set.exists (fun st -> not (isStDeter st representation.transitions) ) 
										representation.allStates in
					not hasNondeterSt		

					
			
			(* partition states by equivalence *)
			method equivalencePartition: states set =
			
				let fa = self#toDeterministic in
				
				let fa2 = fa#cleanUselessStates in
				
				
				let rep = fa2#representation in	
								
                let (inF, notF) = Set.partition (fun x -> Set.belongs x rep.acceptStates) rep.allStates in				
                let distI1 = Set.combinations inF notF in
								
				let hasTransMulti sts sy ts = Set.partition (fun st -> hasTrans st sy ts) sts in				
				let distI2 = Set.flatMap (fun sy -> Util.distrib2 Set.combinations 
													(hasTransMulti rep.allStates sy rep.transitions)) 
								rep.alphabet in
				
				
                let distI = Set.union distI1 distI2 in		
				
                let stsXSts = Set.combinations rep.allStates rep.allStates in		

				(* generates all pairs of states that can reach the pair (st1,st2) through a transition with symbol sy *)
				let reachingSts st1 st2 sy p = 
					let t1 = Set.filter (fun (_,y,z) -> z = st1 && y = sy) rep.transitions in
					let t2 = Set.filter (fun (_,y,z) -> z = st2 && y = sy) rep.transitions in
					let s1 = transitionGet1 t1 in
					let s2 = transitionGet1 t2 in
						Set.diff (Set.combinations s1 s2) p in
								
				let findAR p q = Set.flatMap (fun (a,b) -> Set.flatMap (fun sy -> reachingSts a b sy p) rep.alphabet) q in
				
				let distA = findAR distI distI in
				
                let rec aped p q = if (q = Set.empty || (Set.union p q) = stsXSts) then Set.union p q
                    else aped (Set.union p q) (findAR (Set.union p q) q ) in
					
                let dist = aped distI distA in
				
												
				(* given for example states a b c d generates (a,a) (a,b) (a,c) (a,d) (b,b) (b,c) (b,d) (c,c) (c,d) (d,d) *)
				let rec halfCombs sts = 
					match sts with 
						[] -> Set.empty
						|x::xs -> Set.union (Set.combinations (Set.make [x]) (Set.make sts)) (halfCombs xs) in
										
				let halfTriang = halfCombs (Set.toList rep.allStates) in				
				
				(* given set of equivalent states dicti, substitutes state st for its leftmost equivalent state according to dicti *)
				let rec translate st dicti = 
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in				
				
				(* the set of equivalent state pairs are those not present in the set of distinct state pairs *)
				let equiv = Set.filter ( fun (a,b) -> not (Set.belongs (a,b) dist) && 
														not (Set.belongs (b,a) dist) ) halfTriang in	
				
				let equivList = Set.toList equiv in
				let hasAny st1 st2 sta stb = (translate st1 equivList) = sta || (translate st2 equivList) = sta 
											|| (translate st1 equivList) = stb || (translate st2 equivList) = stb in
				
				
				let rec agroup eq =
					match eq with
						[] -> Set.empty
						|(a,b)::ls ->   let (part1,part2) = Set.partition (fun (x,y) -> hasAny x y a b) (Set.make eq) in						
										let gRemain = Set.flatMap (fun (c,d) -> Set.make [c;d]) part1 in										
											Set.add (Set.union (Set.make [a;b]) gRemain) (agroup (Set.toList part2))
				in
				
				agroup equivList
				
				
			
			
			(** 
			* This method minimizes the automaton 
			*
			* @returns FiniteAutomaton.model -> the new minimal equivalent automaton 
			*
			* Desc: The given automaton is minimized according to the process described in lecture a15.   
			*)
			method minimize: FiniteAutomaton.model = 					
				
				let fa = self#toDeterministic in
				
				let fa2 = fa#cleanUselessStates in
				
				
				let rep = fa2#representation in	
								
                let (inF, notF) = Set.partition (fun x -> Set.belongs x rep.acceptStates) rep.allStates in				
                let distI1 = Set.combinations inF notF in
								
				let hasTransMulti sts sy ts = Set.partition (fun st -> hasTrans st sy ts) sts in				
				let distI2 = Set.flatMap (fun sy -> Util.distrib2 Set.combinations 
													(hasTransMulti rep.allStates sy rep.transitions)) 
								rep.alphabet in
				
				
                let distI = Set.union distI1 distI2 in		
				
                let stsXSts = Set.combinations rep.allStates rep.allStates in		

				(* generates all pairs of states that can reach the pair (st1,st2) through a transition with symbol sy *)
				let reachingSts st1 st2 sy p = 
					let t1 = Set.filter (fun (_,y,z) -> z = st1 && y = sy) rep.transitions in
					let t2 = Set.filter (fun (_,y,z) -> z = st2 && y = sy) rep.transitions in
					let s1 = transitionGet1 t1 in
					let s2 = transitionGet1 t2 in
						Set.diff (Set.combinations s1 s2) p in
								
				let findAR p q = Set.flatMap (fun (a,b) -> Set.flatMap (fun sy -> reachingSts a b sy p) rep.alphabet) q in
				
				let distA = findAR distI distI in
				
                let rec aped p q = if (q = Set.empty || (Set.union p q) = stsXSts) then Set.union p q
                    else aped (Set.union p q) (findAR (Set.union p q) q ) in
					
                let dist = aped distI distA in
				
								
				(* given for example states a b c d generates (a,b) (a,c) (a,d) (b,c) (b,d) (c,d) *)
				let rec halfCombs sts = 
					match sts with 
						[] -> Set.empty
						|x::xs -> Set.union (Set.combinations (Set.make [x]) (Set.make xs)) (halfCombs xs) in
				let halfTriang = halfCombs (Set.toList rep.allStates) in				
				
				(* given set of equivalent states dicti, substitutes state st for its leftmost equivalent state according to dicti *)
				let rec translate st dicti = 
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in				
				
				(* the set of equivalent state pairs are those not present in the set of distinct state pairs *)
				let equiv = Set.filter ( fun (a,b) -> not (Set.belongs (a,b) dist) && 
														not (Set.belongs (b,a) dist) ) halfTriang in	
				
				let equivList = Set.toList equiv in
									
				let eq = Set.map (fun (a,b) -> b) equiv in
				let newSts = Set.diff rep.allStates eq in
				let newInitSt = translate rep.initialState equivList in
				let newAccSts = Set.inter rep.acceptStates newSts in
				let newTrans = Set.map (fun (x,y,z) -> (translate x equivList,y,translate z equivList) ) rep.transitions in
				
				
				new FiniteAutomaton.model (Representation {alphabet = rep.alphabet; allStates = newSts; 
					initialState = newInitSt; transitions = newTrans; acceptStates = newAccSts} ) 
							
			
			(** 
			* This method verifies if the automaton is minimal 
			*
			* @returns boolean -> true if automaton is minimal, false otherwise
			*
			* Desc: The given automaton is considered minimal if the result of minimizing it is an automaton with the same      
			* number of states
			*)
			method isMinimized: bool = 
			
				let fa = self#minimize in
				let rep = fa#representation in
					Set.size representation.allStates = Set.size rep.allStates
					
	
			(** 
			* This method converts the automaton into a regular expression that accepts its language, by
			* using the transitive closure algorithm 
			*
			* @returns RegularExpression.model -> the resulting regular expression
			*
			* Desc: The resulting expression is not minimal
			*)			
			method toRegularExpression =
				(* Since the algorithm only works for deterministic automaton, we first convert it
					to its deterministic equivalent *)
				let fa = self#toDeterministic in
				
				let rep = fa#representation in	
				
				let sts = rep.allStates in
				let trns = rep.transitions in
				
				(* transforms the set of expressions into the regex: plus of all expressions of the set *)			
				let rec plusSet reSet =
					let rec pls l =
						match l with
							[] -> RegExpSyntax.Zero
							| x::xs -> if xs = [] then x else RegExpSyntax.Plus (x, pls xs)
					in
						pls (Set.toList reSet)
				in
				
				(* For the given i and j, returns the value of R when k is zero.
					Note that k will always be 0 when called inside this method *)
				let calczerok k i j = 
					let ts = Set.filter (fun (a,_,b) -> i = a && j = b) trns in
					if ts <> Set.empty then
						if i <> j then 
							let res = Set.map (fun (_,c,_) -> RegExpSyntax.Symb c) ts in 
								(k,i,j,plusSet res)								
						else 
							let res = Set.map (fun (_,c,_) -> RegExpSyntax.Symb c) ts in 
							let re = RegExpSyntax.Plus(RegExpSyntax.Empty, (plusSet res)) in
								(k,i,j,re)
								
					else (k,i,j,RegExpSyntax.Zero)
				in
				
				
				(* For the given i and j, returns the value of R when k is not zero. *)
				let calck k i j prvK = 
					let getRij i j = 
						let r = Set.nth (Set.filter (fun (_,x,y,_) -> x = i && y = j) prvK) 0 in
							(fun (_,_,_,re) -> re) r
					in
					let assembleRe st i j =
						let rik = getRij i st in
						let rkk = RegExpSyntax.Star (getRij st st) in
						let rkj = getRij st j in						
							RegExpSyntax.Seq(rik, RegExpSyntax.Seq(rkk,rkj)) 
					in
					
					let rij = getRij i j in
					let rikjs = Set.map (fun st -> assembleRe st i j) sts in
					let rikj = plusSet rikjs in
						(k,i,j,RegExpSyntax.Plus(rij,rikj)) 
						
				in					
					
				(* Main function that applies previous 2 functions to all possible i and j pairs *)	
				let rec rkij k = 
					if k < 1 then
						Set.map (fun (i,j) -> calczerok k i j) (Set.combinations sts sts)
					else 
						let prvK = rkij (k-1) in
							Set.map (fun(i,j) -> calck k i j prvK) (Set.combinations sts sts)
				in
				
				let allRks = rkij (Set.size sts) in 
				let result = Set.filter (fun (_,i,j,_) -> i = rep.initialState && Set.belongs j rep.acceptStates ) allRks in
				let res = Set.map (fun (_,_,_,re) -> re) result in
				let newRe = plusSet res in
				
					new RegularExpression.model (Representation (newRe))
				

				
			(** 
			* This method converts the automaton into its equivalent regular grammar 
			*
			* @returns ContextFreeGrammar.model -> the resulting regular grammar			
			*)
			method toRegularGrammar = 		
				
				let re = self#toRegularExpression in
				
					re#toRegularGrammar
			
			
					
				
			
		end

end

and (*module*) FiniteAutomatonTests : sig
end
 =
struct
	let active = true

	let test0 () =
		let m = Model.loadModel "test automaton/fa_abc.json" in
			let j = m#toJSon in
				JSon.show j

	let test1 () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_abc.json") in
			let j = fa#toJSon in
				JSon.show j
				
	let testBug () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_abc.json") in
		let fa2 = fa#toDeterministic in
			let j = fa2#toJSon in
				JSon.show j;
		let fa3 = fa2#cleanUselessStates in
			let j = fa3#toJSon in	
				JSon.show j
				
	let testBug2 () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_abc.json") in
		let fa2 = fa#toDeterministic in
			Util.println "productive states:"; Util.printStates (Set.toList fa2#productive); Util.println " "
				

	let testAcceptBF () =
        let fa = new FiniteAutomaton.model (File "test automaton/fa_accept.json") in
          	if fa#acceptBreadthFirst [] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['a';'b'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['b'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['b';'a'] then Util.println "word was accepted" else Util.println "word was not accepted";
			if fa#acceptBreadthFirst ['a';'b';'b'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['a';'b';'a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['b';'a';'a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['b';'a';'b'] then Util.println "word was accepted" else Util.println "word was not accepted";
			Util.println ""

	let testAcceptBF2 () =
        let fa = new FiniteAutomaton.model (File "test automaton/fa_accept2.json") in
          	if fa#acceptBreadthFirst [] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['a';'d'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['a';'b';'a';'d'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#acceptBreadthFirst ['c'] then Util.println "word was accepted" else Util.println "word was not accepted";
			Util.println ""
				
	let testAccept () =
        let fa = new FiniteAutomaton.model (File "test automaton/fa_accept.json") in
          	if fa#accept [] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['a';'b'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['b'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['b';'a'] then Util.println "word was accepted" else Util.println "word was not accepted";
			if fa#accept ['a';'b';'b'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['a';'b';'a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['b';'a';'a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['b';'a';'b'] then Util.println "word was accepted" else Util.println "word was not accepted";
			Util.println ""
			
	let testAccTrace () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_abc.json") in
			fa#acceptWithTracing ['a';'b';'e'] 

			
	let testAccept2 () =
        let fa = new FiniteAutomaton.model (File "test automaton/fa_accept2.json") in
          	if fa#accept [] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['a';'d'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['a';'b';'a';'d'] then Util.println "word was accepted" else Util.println "word was not accepted";
          	if fa#accept ['c'] then Util.println "word was accepted" else Util.println "word was not accepted";
			Util.println ""
			
			
	
	let testGenerate () =
  		let fa = new FiniteAutomaton.model (File "test automaton/fa_generate.json") in
			Util.println "generated words size 0:"; Util.printWordList (Set.toList (fa#generate 0) );
  		    Util.println "generated words size 1:"; Util.printWordList (Set.toList (fa#generate 1) );
  		    Util.println "generated words size 2:"; Util.printWordList (Set.toList (fa#generate 2) );			
  		    Util.println "generated words size 100:"; Util.printWordList (Set.toList (fa#generate 100) );
			Util.println ""
			
	let testGenerate2 () =
  		let fa = new FiniteAutomaton.model (File "test automaton/fa_generate2.json") in
			Util.println "generated words size 0:"; Util.printWordList (Set.toList (fa#generate 0));
  		    Util.println "generated words size 1:"; Util.printWordList (Set.toList (fa#generate 1));
  		    Util.println "generated words size 2:"; Util.printWordList (Set.toList (fa#generate 2));	
			Util.println "generated words size 3:"; Util.printWordList (Set.toList (fa#generate 3) );
			Util.println "generated words size 4:"; Util.printWordList (Set.toList (fa#generate 4) );
  		    Util.println "generated words size 18:"; Util.printWordList (Set.toList (fa#generate 18) );

			Util.println ""
			
	let testGenerate3 () =
  		let fa = new FiniteAutomaton.model (File "test automaton/fa_generate3.json") in
			Util.println "generated words size 0:"; Util.printWordList (Set.toList (fa#generate 0));
  		    Util.println "generated words size 1:"; Util.printWordList (Set.toList (fa#generate 1));
  		    Util.println "generated words size 10:"; Util.printWordList (Set.toList (fa#generate 10));	
			Util.println "generated words size 50:"; Util.printWordList (Set.toList (fa#generate 50));
			Util.println "generated words size 100:"; Util.printWordList (Set.toList (fa#generate 100));
			Util.println "generated words size 1000:"; Util.printWordList (Set.toList (fa#generate 1000));
			Util.println ""
			
	let testGenerate4 () =
  		let fa = new FiniteAutomaton.model (File "test automaton/fa_generate4.json") in
			Util.println "generated words size 0:"; Util.printWordList (Set.toList (fa#generate 0));
  		    Util.println "generated words size 1:"; Util.printWordList (Set.toList (fa#generate 1));
  		    Util.println "generated words size 10:"; Util.printWordList (Set.toList (fa#generate 10));
			Util.println "generated words size 100:"; Util.printWordList (Set.toList (fa#generate 100));				
			Util.println ""
			
	let testGenerateUntil () =
  		let fa = new FiniteAutomaton.model (File "test automaton/fa_generate.json") in
			Util.println "generated words size 5:"; Util.printWordList (Set.toList (fa#generateUntil 5));
			Util.println "";
		let fa = new FiniteAutomaton.model (File "test automaton/fa_generate2.json") in
			Util.println "generated words size 5:"; Util.printWordList (Set.toList (fa#generateUntil 5));
			Util.println "";
		let fa = new FiniteAutomaton.model (File "test automaton/fa_generate3.json") in
			Util.println "generated words size 5:"; Util.printWordList (Set.toList (fa#generateUntil 5));
			Util.println "";
		let fa = new FiniteAutomaton.model (File "test automaton/fa_generate4.json") in
			Util.println "generated words size 5:"; Util.printWordList (Set.toList (fa#generateUntil 5));
			Util.println ""
			

    let testReachable () =
      		let fa = new FiniteAutomaton.model (File "test automaton/fa_reach.json") in
			let fa2 = new FiniteAutomaton.model (File "test automaton/fa_reach2.json") in
			let start = fa#representation.initialState in
			let start2 = fa2#representation.initialState in
      		    Util.println "reachable states:"; Util.printStates (Set.toList (fa#reachable start)); Util.println " ";
				Util.println "reachable states:"; Util.printStates (Set.toList (fa#reachable start2)); Util.println " "

    let testProductive () =
        let fa = new FiniteAutomaton.model (File "test automaton/fa_productive.json") in
		let fa2 = new FiniteAutomaton.model (File "test automaton/fa_productive2.json") in
      		Util.println "productive states:"; Util.printStates (Set.toList (fa#productive)); Util.println " ";
			Util.println "productive states:"; Util.printStates (Set.toList (fa2#productive)); Util.println " "
			
	let testClean () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_clean.json") in
		let fa2 = new FiniteAutomaton.model (File "test automaton/fa_clean2.json") in
		let mfa = fa#cleanUselessStates in
		let mfa2 = fa2#cleanUselessStates in
		let j = mfa#toJSon in
		let j2 = mfa2#toJSon in
			JSon.show j; Util.println " ";
			JSon.show j2; Util.println " "
			

    let testIsDeterministic () =
        let fa = new FiniteAutomaton.model (File "test automaton/fa_isDeter.json") in
		let fa2 = new FiniteAutomaton.model (File "test automaton/fa_isDeter2.json") in
		let fa3 = new FiniteAutomaton.model (File "test automaton/fa_isDeter3.json") in
          	if fa#isDeterministic then
          		Util.println "automata is deterministic" else Util.println "automata is non-deterministic";
			if fa2#isDeterministic then
          		Util.println "automata is deterministic" else Util.println "automata is non-deterministic";
			if fa3#isDeterministic then
          		Util.println "automata is deterministic" else Util.println "automata is non-deterministic"


    
	let testToDeterministic () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_toDeter.json") in
		let mfa = fa#toDeterministic in
		let j = mfa#toJSon in		
			JSon.show j;			
		let fa = new FiniteAutomaton.model (File "test automaton/fa_isDeter.json") in
		let mfa = fa#toDeterministic in
		let j = mfa#toJSon in
			JSon.show j
			
			
	let testEquivalence () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_abc.json") in
		let s = fa#equivalencePartition in
			Set.iter (fun s -> print_string "set: "; Util.printStates (Set.toList s)) s 
			
			
	let testMinimize () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_minimize.json") in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j
	
	let testMinimize2 () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_minimize2.json") in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j
	
	let testMinimize3 () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_minimize3.json") in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j
			
	let testMinimize4 () =
		let fa = new FiniteAutomaton.model (File "test automaton/fa_minimize4.json") in
		let mfa = fa#minimize in
		let j = mfa#toJSon in
			JSon.show j
	
	
	let runAll =
		if active then (
			Util.header "FiniteAutomatonTests";
			testEquivalence ()
		)
end


(* --- Regular Expression --- *)

and (*module*) RegularExpression : sig

	type reTree =
    | Fail 
    | Tree of word * RegExpSyntax.t * reTree list



	type t = RegExpSyntax.t
	val modelDesignation: unit -> string
	class model :
	  t JSon.alternatives ->
		  object
			method kind : string
			method description : string
			method name : string
			method errors : string list
			method handleErrors : unit
			method validate : unit
			method toJSon: json

			method accept : word -> bool
			method allTrees : word -> unit
			method generate : int -> words
			method tracing : unit
			
			method alphabet : symbols
			method quasiLanguage : words 

			method simplify : RegularExpression.model
			method toFiniteAutomaton : FiniteAutomaton.model
			method toRegularGrammar : ContextFreeGrammar.model 
			
			method representation : t
			method checkExercise : Exercise.exercise -> bool
			method checkExerciseFailures : Exercise.exercise -> (words * words) 

			
		  end
end
 =
struct
	open RegExpSyntax
	open ContextFreeGrammar
	open CFGSyntax
	
	type reTree =
    | Fail 
    | Tree of word * RegExpSyntax.t * reTree list

	
	
	type t = RegExpSyntax.t
	

	let modelDesignation () = "regular expression"
	
	(* auxiliary functions *)
	let seqConcat aset bset = Set.flatMap (fun s1 -> Set.map (fun s2 -> s1@s2) bset) aset 
	

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

			method representation = representation	
				
			method validate = ( 
				
				(*
				let representation = RegExpSyntax.parse "(((xx+ut)+(aaa+dss+ghf)+(xx+uu))ee)+bgc*+(jgg+bgcd)" in 
				 
				let rec lang rep = 
					match rep with 
						| RegExpSyntax.Plus(l, r) -> print_string "pls: "; print_string (RegExpSyntax.toString l); print_string ", ";
						print_string (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Seq(l, r) -> print_string "seq: "; print_string (RegExpSyntax.toString l); print_string ", ";
						print_string (RegExpSyntax.toString r); Util.println ""; Set.union (lang l) (lang r)
						| RegExpSyntax.Star(r) -> print_string "str: "; print_string (RegExpSyntax.toString r); Util.println ""; (lang r)
						| RegExpSyntax.Symb(c) -> Set.make [c]
						| RegExpSyntax.Empty -> Set.empty
						| RegExpSyntax.Zero -> Set.empty
				in
				let a = lang representation in
				()
				*)
			)

									
			method tracing: unit = ()

				
			(** 
			* This method generates the alphabet of all symbols in the expression
			*
			* @returns symbols -> the set of all symbols in the expression's alphabet
			*
			*)					
			method alphabet: symbols =						
							
				let rec alf rep = 
					match rep with 
						| Plus(l, r) -> Set.union (alf l) (alf r) 
						| Seq(l, r) -> Set.union (alf l) (alf r)
						| Star(r) -> alf r
						| Symb(c) -> Set.make [c]
						| Empty -> Set.empty
						| Zero -> Set.empty
				in
					alf representation
			
			
			(** 
			* This method generates the language of the regular expression for when klenne is always zero
			*
			* @returns words -> set of generated words
			*
			*)	
			method quasiLanguage: words =
													
				let rec lang rep = 
					match rep with 
						| Plus(l, r) -> Set.union (lang l) (lang r)	
						| Seq(l, r) ->	 seqConcat (lang l) (lang r)											
						| Star(r) -> Set.make [[]]
						| Symb(c) -> Set.make [[c]]
						| Empty -> Set.empty
						| Zero -> Set.empty
				in
					lang representation 
				
			
				
				
			(** 
			* This method tests if a given word is accepted by the regular expression 
			*
			* @param w:word -> word to be tested for acceptance 
			*
			* @returns bool -> true if w is accepted and false otherwise
			*
			*)	
			method accept (w: word): bool = 
				
				let partition w = 
					let rec partX w pword =
						match w with 
							[] -> Set.empty
							| x::xs -> let fwp = pword@[x] in
											Set.add (fwp, xs) (partX xs fwp) in
						 Set.add ([],w) (partX w []) in									
				
				let rec acc rep w = 				
					match rep with 
						| Plus(l, r) -> (acc l w) || (acc r w)		
						| Seq(l, r) -> let wpl =  partition w in
										   Set.exists (fun (wp1,wp2) -> (acc l wp1) && (acc r wp2)) wpl											
						| Star(re) -> w = [] ||							 
									  (let wpl = Set.remove ([],w) (partition w) in
											Set.exists (fun (wp1,wp2) -> (acc re wp1) && (acc (Star re) wp2)) wpl)	
						| Symb(c) -> w = [c]  
						| Empty -> w = []
						| Zero -> false
				in	
					
					acc representation w

				
				
			(** 
			* This method returns the derivation tree for the word acceptance
			*
			* @param w:word -> word to be tested for acceptance 
			*
			* @returns reTree list -> list of derivation trees 
			*
			*)		
			method allTrees w : unit = 
			
			
				let partition w = 
					let rec partX w pword =
						match w with 
							[] -> Set.empty
							| x::xs -> let fwp = pword@[x] in
										Set.add (fwp, xs) (partX xs fwp) 
					in
					Set.add ([],w) (partX w []) 
				in         

				let rec acc w rep =                             
					match rep with 
						| Plus(l, r) -> 
							let l1 = acc w l in
							let r1 = acc w r in 
								List.map (fun t -> Tree (w, rep, [t])) (l1 @ r1)
					
						| Seq(l, r) -> 
							let wps =  partition w in
							let wpl = Set.toList wps in 
							List.flatten ( List.map (fun (wp1, wp2) ->  
								let tl = acc wp1 l in
								let tr = acc wp2 r in
									List.flatten (List.map (fun x -> List.map 
										(fun y -> Tree (w, rep, [x; y])) tr)tl)
							) wpl)
							
						| Star(re) -> 
							if w = [] then 
								[Tree (['~'], rep, [])] 
							else                                             
								(let wps = Set.remove ([],w) (partition w) in
								let wpl = Set.toList wps in 
								List.flatten (List.map (fun (wp1, wp2) ->  
									let tl = acc wp1 re in
									let tr = acc wp2 (Star re) in
									List.flatten (List.map (fun x -> List.map 
										(fun y -> Tree (w, rep, [x; y])) tr) tl)) wpl))
										
						| Symb(c) -> 
							if w = [c] then 
								[Tree (w, rep, [])]
							else 
								[Tree (w, rep, [Fail])]
							
						| Empty -> 
							if w = [] then
								[Tree (['~'], rep, [])]
							else
								[Tree (w, rep, [Fail])]
							
						| Zero -> [Tree (w, rep, [Fail])]
					
				in      

				let ac = acc w self#representation in
				
				
						
				let rec isNotFail t = 
					match t with
						Fail -> false
						| Tree ([], re, []) -> true
						| Tree (w, re, []) -> true
						| Tree ([], re, x::xs) -> (isNotFail x) && (isNotFail (Tree ([], re, xs)))
						| Tree (w, re, x::xs) -> (isNotFail x) && (isNotFail (Tree (w, re, xs)))
				in
									
				let ts = List.filter (fun t -> isNotFail t) ac in
				
				
				let printTreeX w re n = 
					let s = String.make (3*n) ' ' in
					print_string s; print_string (Util.wordToString w); print_string " -> "; 
					print_string (RegExpSyntax.toString re); Util.println " ";
				in
				
				let rec printTree t n = 
					match t with
						Fail -> Util.println "Fail"
						| Tree ([], re, []) -> Util.println "TREH "
						| Tree (w, re, []) -> printTreeX w re n
						| Tree ([], re, x::xs) -> printTreeX [] re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
						| Tree (w, re, x::xs) -> printTreeX w re n; printTree x (n+1); List.iter (fun t -> printTree t (n+1)) xs
				in
				
					List.iter (fun t -> printTree t 0) ts
				
					
					
			(** 
			* This method generates all words up to the given length that are generated by the regular expression 
			*
			* @param length:int -> maximum length of all generated words
			*
			* @returns words -> set of generated words
			*)	
			method generate (length: int): words = 
												
				let rec lang rep ln = 
					match rep with 
						| Plus(l, r) -> Set.union (lang l ln) (lang r ln)	
						| Seq(l, r) ->  let left = lang l ln in
										let rigth w = lang r (ln - (List.length w)) in
										let conc w = Util.concatAll w (Set.toList (rigth w)) in
											Set.flatMap (fun lw -> Set.make (conc lw)) left											
						| Star r -> let exp = lang r ln in
										Set.star exp ln
				
							(* alternate version of star, leave 4 now
							
							let rec starX ws sz = 
								if sz <= 0 then Set.make [[]] 
								else 
									let ws = Set.filter (fun x -> sz >= (List.length x)) ws in
									let newLn w = sz - (List.length w) in					
									let tail w ws = Set.toList (starX ws (newLn w)) in
									let conc w ws = Util.concatAll w (tail w ws) in
									let track w ws = Set.add w (Set.make (conc w ws)) in
										Set.flatMap (fun w -> if w = [] then Set.make [[]] else track w ws) ws in										
							let exp = lang r ln in
								Set.add [] (starX exp ln)*)
										
						| Symb(c) -> if ln > 0 then Set.make [[c]] else Set.empty
						| Empty -> Set.make [[]]
						| Zero -> Set.empty 
				in  
					lang representation length
			
			
			(** 
			* This method simplifies the regular expression
			*
			* @returns RegularExpression.model -> the new simplified, equivalent expression 
			*)	
			method simplify : RegularExpression.model = 
								
				(* various base case simplification rules to apply to the given expressions *)
				let rec simpX re = 
					match re with	
						(* plus *)	
						(* a* + empty *)
						| Plus(Star(l), Empty) -> Star(l)	
						| Plus(Empty, Star(r)) -> Star(r)
						(* a* + zero *)
						| Plus(Zero, r) -> r
						| Plus(l, Zero) -> l
						(* ~ + aa* *)
						| Plus(Empty, Seq(l, Star(r))) when l = r -> Star(r)
						| Plus(Empty, Seq(Star(l), r)) when l = r -> Star(l)
						| Plus(Seq(l, Star(r)), Empty) when l = r -> Star(r)
						| Plus(Seq(Star(l), r), Empty) when l = r -> Star(l)
						(* a* + a + empty  *)
						| Plus(Star(l), Plus(Empty, r)) when l = r -> Star(l)
						| Plus(Star(l), Plus(r, Empty)) when l = r -> Star(l)
						| Plus(Plus(Empty, l), Star(r)) when l = r -> Star(r)
						| Plus(Plus(l, Empty), Star(r)) when l = r -> Star(r)
						(* a* + a *)											
						| Plus(Star(l), r) when l = r -> Star(l)
						| Plus(l, Star(r)) when l = r -> Star(r)							
						(* a + b = a||b when a = b *)
						| Plus(l, r) when l = r -> l 
						(* seq *)							
						| Seq(Empty, Empty) -> Empty
						| Seq(Zero, Zero) -> Zero
						| Seq(Empty, r) -> r	
						| Seq(l, Empty) -> l	
						| Seq(Zero, r) -> Zero	
						| Seq(l, Zero) -> Zero	
						(* (~+a)a* *)						
						| Seq(Plus(Empty, l),Star(r)) when l = r -> Star(r)
						| Seq(Plus(l, Empty),Star(r)) when l = r -> Star(r)
						| Seq(Star(l),Plus(Empty, r)) when l = r -> Star(l)
						| Seq(Star(l),Plus(r, Empty)) when l = r -> Star(l)
						| Seq(Star(l),Star(r)) when l = r -> Star(l)
						(* star *)
						| Star(Star(r)) -> Star(r)
						| Star(Plus(Empty, r)) -> Star(r)
						| Star(Plus(r, Empty)) -> Star(r)
						| Star(Empty) -> Empty
						| Star(Zero) -> Empty
						| Star(r) -> re
						(* symb *)
						| Symb(c) -> Symb c
						(* empty *)
						| Empty -> Empty
						(* zero *)
						| Zero -> Zero
						| _ -> re
				in
			
				(* applies various base case simplifications to the various sub-expressions of regular expression re *)
				let rec simplify re =
					
					match re with	
						| Plus(l,r) -> simpX (Plus(simplify l, simplify r))
						| Seq(l,r) -> simpX (Seq(simplify l, simplify r))
						| Star(re) -> simpX (Star(simplify re))
						| Symb(c) -> Symb c
						| Empty -> Empty
						| Zero -> Zero
				in			
			
				let sre = simplify representation in
			
				new RegularExpression.model (Representation (sre))
				
				
			
			(* This method converts the regular expression to its equivalent finite automaton 
			*
			* @returns FiniteAutomaton.model -> the resulting finite automaton  
			*)	
			method toFiniteAutomaton: FiniteAutomaton.model = 
				(*auxiliary var for genName function*)				
				let k = ref 0 in
				
				(*for each new state, generates a name that will distinguish it from all the other generated states *)
				let genName () =
					let n = !k in 
					let () = k:= n + 1 in
						(*easy way of having all single digit state names have a zero before their actual number*)
						if n > 9 then "new_St" ^ (string_of_int n) 
									else "new_St0" ^ (string_of_int n) in
						
								
				let rec compile (rep: RegularExpression.t) : FiniteAutomaton.t = 
					match rep with 
						| Plus(l, r) -> let fa1 = compile l in
										let fa2 = compile r in
										let newStart = genName () in
										let newSts = Set.add newStart (Set.union fa1.allStates fa2.allStates) in
										let newAccSts = Set.union fa1.acceptStates fa2.acceptStates in
										let newTran1 = (newStart, epsilon, fa1.initialState) in
										let newTran2 = (newStart, epsilon, fa2.initialState) in
										let newTrans = Set.add newTran1 (Set.add newTran2 
											(Set.union fa1.transitions fa2.transitions)) in
										let newAlf = Set.union fa1.alphabet fa2.alphabet in
										
											{alphabet = newAlf; allStates = newSts; initialState = newStart; 
												transitions = newTrans; acceptStates = newAccSts} 
												
						| Seq(l, r) ->	let fa1 = compile l in
										let fa2 = compile r in
										let ist = fa1.initialState in
										let sts = Set.union fa1.allStates fa2.allStates in
										let asts = fa2.acceptStates in										
										let newTrns = Set.map (fun x -> (x, epsilon, fa2.initialState) ) fa1.acceptStates in										
										let trns = Set.union newTrns (Set.union fa1.transitions fa2.transitions) in
										let alf = Set.union fa1.alphabet fa2.alphabet in
						
											{alphabet = alf; allStates = sts; initialState = ist; 
												transitions = trns; acceptStates = asts} 
						
						| Star(r) -> let fa = compile r in
									 let newStart = genName () in
									 let newSts = Set.add newStart fa.allStates in
									 let newTrns = Set.map (fun st -> (st, epsilon, newStart)) fa.acceptStates in
									 let allNewTrns = Set.add (newStart, epsilon, fa.initialState) (Set.union newTrns fa.transitions) in
									 
										{alphabet = fa.alphabet; allStates = newSts; initialState = newStart; 
											transitions = allNewTrns; acceptStates = Set.make [newStart]}   
						
						| Symb(c) -> let newStart = genName () in
									 let newAcc = genName () in
									 let newSts = Set.make [newStart; newAcc] in
									 let newTrn = Set.make [(newStart, c, newAcc)] in
									 
										{alphabet = Set.make [c]; allStates = newSts; initialState = newStart; 
											transitions = newTrn; acceptStates = Set.make [newAcc]} 
									
						| Empty -> let newStart = genName () in
						
										{alphabet = Set.empty; allStates = Set.make [newStart]; initialState = newStart; 
											transitions = Set.empty; acceptStates = Set.make [newStart]}   
									
						| Zero -> let newStart = genName () in
						
									{alphabet = Set.empty; allStates = Set.make [newStart]; initialState = newStart; 
											transitions = Set.empty; acceptStates = Set.empty}   	
				in
				
					new FiniteAutomaton.model (Representation (compile representation)  ) 
			
			
			
			
			(* This method converts the regular expression to its equivalent regular grammar 
			*
			* @returns FiniteAutomaton.model -> the resulting regular grammar 
			*)	
			method toRegularGrammar =
						
				(*auxiliary var for genVar function*)				
				let k = ref 0 in
				
				(* generates new unused variable name for the cfg *)
				let genVar () =
					let n = !k in 
					let () = k:= n + 1 in
					let ascii = 65 + n in
					if ascii < 65 || ascii > 90 then 'A' 
						else Char.chr ascii
				in	
			
				(* 
				let convertPlsRules rl i1 i2 newInit  = 
					(* swaps the initial variables of both old cfgs for the new initial var  *)
					let swapInits c = if c = i1 || c = i2 then newInit else c in
					
					let newBody b = List.map (fun c -> swapInits c) b in					
					let newRule r = {head = swapInits r.head; body = newBody r.body} in						
						
						Set.map (fun r -> newRule r) rl 
					
				in	
				*)

				(* create gcf rules for plus expression *)
				let convertPlsRules rl i1 i2 newInit  = 
				
					let newRule1 = {head = newInit; body = [i1]} in
					let newRule2 = {head = newInit; body = [i2]} in			
						
						Set.add newRule1 (Set.add newRule2 rl)  
					
				in				
				
				(* create gcf rules for seq expression *)
				let convertSeqRules lcfg rcfg = 				
					let rl1 = lcfg.rules in
					let rl2 = rcfg.rules in
					let alp1 = lcfg.alphabet in				
					let rl = Set.union rl1 rl2 in
					
					let newBody r = 
						let b = r.body in
							match b with
								| [c] when Set.belongs r rl1 && not (Set.belongs c alp1) && c <> epsilon -> b
								| [c] when Set.belongs r rl1 && Set.belongs c alp1 -> [c; rcfg.initial]
								| [epsilon] when Set.belongs r rl1 -> [epsilon; rcfg.initial]
								| b when Set.belongs r rl2 -> b
								| _ -> b
					in					
					let newRule r = {head = r.head; body = newBody r} in									
						Set.map (fun r -> newRule r) rl 
				in
				
				(* create gcf rules for star expression *)
				let convertStrRules cfg =
				
					let newBody b =
						match b with
							| [c] when Set.belongs c cfg.alphabet -> [c; cfg.initial]
							| _ -> b
					in					
					let r0 = {head = cfg.initial; body = [epsilon]} in
					 
					let newRule r = {head = r.head; body = newBody r.body} in									
					let newRules = Set.map (fun r -> newRule r) cfg.rules in
						Set.add r0 newRules 													
				in
				
				
				
				let rec compile rep = 
					match rep with 
					
						| Plus(l, r) -> let cl = compile l in
										let cr = compile r in
										let alp = Set.union cl.alphabet cr.alphabet in
										let init = genVar () in
										let vs = Set.add init (Set.union cl.variables cr.variables) in	
										let rl = Set.union cl.rules cr.rules in
										let rl = convertPlsRules rl cl.initial cr.initial init in
										
											{alphabet = alp; variables = vs; 
												initial = init; rules = rl}		

						| Seq(l, r) ->  let cl = compile l in
										let cr = compile r in
										let alp = Set.union cl.alphabet cr.alphabet in
										let init = cl.initial in
										let vs = Set.union cl.variables cr.variables in		
										let rl = convertSeqRules cl cr in
										
											{alphabet = alp; variables = vs; 
												initial = init; rules = rl}		
												
						| Star(re) ->   let cre = compile re in
									    let alp = cre.alphabet in
										let init = cre.initial in
										let vs = cre.variables in									
										let rl = convertStrRules cre in 
										
											{alphabet = alp; variables = vs; 
												initial = init; rules = rl}		 
												
					
						| Symb(c) -> let alp = Set.make [c] in
									 let init = genVar () in
									 let vars = Set.make [init] in									 
									 let rules = Set.make [{head = init; body = [c]}] in
									 
										{alphabet = alp; variables = vars; 
											initial = init; rules = rules}
											
						| Empty ->  let alp = Set.empty in
									let init = genVar () in
									let vars = Set.make [init] in		
									let rules = Set.make [{head = init; body = [epsilon]}] in
										{alphabet = alp; variables = vars; 
											initial = init; rules = rules}
											
						| Zero -> 	 let alp = Set.empty in
									 let init = genVar () in
									 let var2 = genVar () in
									 let vars = Set.make [init; var2] in	
									 let r1 = {head = init; body = [var2]} in
									 let r2 = {head = var2; body = [init]} in
									 let rules = Set.make [r1; r2] in
									 									 
										{alphabet = alp; variables = vars; 
											initial = init; rules = rules}
				in
			
							
				let cfg = compile representation in
				
					new ContextFreeGrammar.model (Representation (cfg))
				
			
			
			
		end

end

and (*module*) RegularExpressionTests: sig
end
=
struct
	let active = false

	let test0 () =
		let m = Model.loadModel "test regEx/re_abc.json" in
			let j = m#toJSon in
				JSon.show j
				
			
	let test1 () =
		let re = new RegularExpression.model (File "test regEx/re_abc.json") in
			let j = re#toJSon in
				JSon.show j			
	
	let testAlphabet () =
		let re = new RegularExpression.model (File "test regEx/re_abc.json") in
			Util.println "alphabet: "; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println ""
			
	let testAlphabet2 () =
		let re = new RegularExpression.model (File "test regEx/re_simple.json") in
			Util.println "alphabet: "; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println ""
			
	let testAlphabet3 () =
		let re = new RegularExpression.model (File "test regEx/re_complex.json") in
			Util.println "alphabet: "; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println ""
	
	let testAlphabet4 () =
		let re = new RegularExpression.model (File "test regEx/re_convoluted.json") in
			Util.println "alphabet: "; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println ""
	
	let testQuasiLang () =
		let re = new RegularExpression.model (File "test regEx/re_abc.json") in
			let ws = re#quasiLanguage in
			Util.printWordList (Set.toList ws)
			
	let testQuasiLang2 () =
		let re = new RegularExpression.model (File "test regEx/re_simple.json") in
			let ws = re#quasiLanguage in
			Util.printWordList (Set.toList ws)
			
	let testQuasiLang3 () =
		let re = new RegularExpression.model (File "test regEx/re_complex.json") in
			let ws = re#quasiLanguage in
			Util.printWordList (Set.toList ws)
			
	let testQuasiLang4 () =
		let re = new RegularExpression.model (File "test regEx/re_convoluted.json") in
			let ws = re#quasiLanguage in
			Util.printWordList (Set.toList ws)
		
	let testAccept () =
		let m = Model.loadModel "test regEx/re_abc.json" in
			if m#accept ['a';'a'] then Util.println "word was accepted" else Util.println "word was not accepted" 
		
	let testAccept2 () =
		let m = Model.loadModel "test regEx/re_simple.json" in
			if m#accept ['a';'a'] then Util.println "word was accepted" else Util.println "word was not accepted" 
		
	let testAccept3 () =
		let m = Model.loadModel "test regEx/re_complex.json" in
			if m#accept ['a';'a'] then Util.println "word was accepted" else Util.println "word was not accepted" 
		
	let testAccept4 () =
		let m = Model.loadModel "test regEx/re_convoluted.json" in
			if m#accept ['a';'a'] then Util.println "word was accepted" else Util.println "word was not accepted" 
		
			
	let testGenerate () =
		let re = new RegularExpression.model (File "test regEx/re_abc.json") in
			Util.println "generated words size 0:"; Util.printWordList (Set.toList (re#generate 0));
  		    Util.println "generated words size 1:"; Util.printWordList (Set.toList (re#generate 1));
  		    Util.println "generated words size 2:"; Util.printWordList (Set.toList (re#generate 2));
			Util.println "generated words size 3:"; Util.printWordList (Set.toList (re#generate 3));
			Util.println "generated words size 4:"; Util.printWordList (Set.toList (re#generate 4));
			Util.println ""
			
	let testGenerate2 () =
		let re = new RegularExpression.model (File "test regEx/re_simple.json") in
			Util.println "generated words size 0:"; Util.printWordList (Set.toList (re#generate 0));
  		    Util.println "generated words size 1:"; Util.printWordList (Set.toList (re#generate 1));
  		    Util.println "generated words size 2:"; Util.printWordList (Set.toList (re#generate 2));
			Util.println "generated words size 3:"; Util.printWordList (Set.toList (re#generate 3));
			Util.println "generated words size 4:"; Util.printWordList (Set.toList (re#generate 4));
			Util.println ""
			
	let testGenerate3 () =
		let re = new RegularExpression.model (File "test regEx/re_complex.json") in
			Util.println "generated words size 0:"; Util.printWordList (Set.toList (re#generate 0));
  		    Util.println "generated words size 1:"; Util.printWordList (Set.toList (re#generate 1));
  		    Util.println "generated words size 2:"; Util.printWordList (Set.toList (re#generate 2));
			Util.println "generated words size 3:"; Util.printWordList (Set.toList (re#generate 3));
			Util.println "generated words size 4:"; Util.printWordList (Set.toList (re#generate 4));
			Util.println ""
			
	let testGenerate4 () =
		let re = new RegularExpression.model (File "test regEx/re_convoluted.json") in
			Util.println "generated words size 0:"; Util.printWordList (Set.toList (re#generate 0));
  		    Util.println "generated words size 1:"; Util.printWordList (Set.toList (re#generate 1));
  		    Util.println "generated words size 2:"; Util.printWordList (Set.toList (re#generate 2));
			Util.println "generated words size 3:"; Util.printWordList (Set.toList (re#generate 3));
			Util.println "generated words size 4:"; Util.printWordList (Set.toList (re#generate 4));
			Util.println ""

	let testToFA () =
		let re = new RegularExpression.model (File "test regEx/re_abc.json") in
		let fa = re#toFiniteAutomaton in
			JSon.show fa#toJSon
			
	let testToFA2 () =
		let re = new RegularExpression.model (File "test regEx/re_simple.json") in
		let fa = re#toFiniteAutomaton in
			JSon.show fa#toJSon
			
	let testToFA3 () =
		let re = new RegularExpression.model (File "test regEx/re_complex.json") in
		let fa = re#toFiniteAutomaton in
			JSon.show fa#toJSon
			
	let testToFA4 () =
		let re = new RegularExpression.model (File "test regEx/re_convoluted.json") in
		let fa = re#toFiniteAutomaton in
			JSon.show fa#toJSon
			
	let testSimplify () =
		let fa = new FiniteAutomaton.model (File "test Automaton/fa_toRe.json") in
		let r = fa#toRegularExpression in
		JSon.show r#toJSon;
		let rs = r#simplify in
		let j = rs#toJSon in
			JSon.show j		
			
	let testSimplify2 () =
		let re = new RegularExpression.model (File "test regEx/re_simple.json") in
		let res = re#simplify in
			JSon.show res#toJSon
			

	let testToRe () =
		let fa = new FiniteAutomaton.model (File "test Automaton/fa_toRe.json") in
		let r = fa#toRegularExpression in
		let j = r#toJSon in
			JSon.show j		

	let testToGrammar () =
		let re = new RegularExpression.model (File "test regEx/re_simple.json") in
		let res = re#toRegularGrammar in
			JSon.show res#toJSon	
	
	
	let testEnum () =
		let e = new Exercise.exercise (File "test enums/enum_astar.json")  in
		let re = new RegularExpression.model (File "test regEx/re_simple.json") in
		let result = re#checkExercise e in
			if result then print_string "it works" else print_string "it does not work"
			
	let testTrace () = 	
		let re = new RegularExpression.model (File "test regEx/re_simple.json") in
			re#allTrees ['a';'c';'b';'a';'c';'b']
		

	let runAll =
		if active then (
			Util.header "RegularExpressionTests";
			testTrace ()
		)
		
				
end


(**)
and (*module*) ContextFreeGrammar : sig
	type cfgTree = Leaf of char | Root of char * cfgTree list 

	type t = {
		alphabet : symbols;
		variables : variables;
		initial : variable;
		rules : CFGSyntax.rules;
	}
	
	val modelDesignation: unit -> string (* a funtion required for module recursive call *)
	class model :
	  t JSon.alternatives ->
		  object
			method kind : string
			method description : string
			method name : string
			method errors : string list
			method handleErrors : unit
			method validate : unit
			method toJSon: json
			method representation: t
			
			method tracing: unit	
			method isRegular: bool
			method accept: word -> bool
			method acceptWithTracing: word -> unit
			method generate: int -> words
			method toFiniteAutomaton: FiniteAutomaton.model
			method toRegularExpression: RegularExpression.model
			method checkExercise: Exercise.exercise -> bool 
			method checkExerciseFailures : Exercise.exercise -> (words * words) 
		  end
end
 =
struct
	type cfgTree = Leaf of char | Root of char * cfgTree list 

	open FiniteAutomaton
	open CFGSyntax
	type t = {
		alphabet : symbols;
		variables : variables;
		initial : variable;
		rules : CFGSyntax.rules;
	}
	
	let modelDesignation () = "context free grammar"	

	(*------Auxiliary functions---------*)
	
	(* given a head, returns the set of all its bodies according to the cfg's rules *)
	let bodiesOfHead h rl =
		let rls = Set.filter (fun r -> r.head = h) rl in
			Set.map (fun r -> r.body) rls 
	
	
	(* given 2 sets of words, to each word of the left set, appends each word of the right set *)
	let concatWords lws rws = 
		if lws = Set.empty then rws
		else if rws = Set.empty then lws
		else					
			let pairs = Set.combinations lws rws  in
				Set.map (fun (x,y) -> x@y) pairs
		
	(* tests if the number of symbols in the given word exceeds the given lenght *)
	let exceedsMaxLen w l alph =
		let cleanWord = List.filter (fun c ->  Set.belongs c alph) w in
			(List.length cleanWord) > l

	

	let subX h rws rl =
		let bs = bodiesOfHead h rl in
			concatWords bs rws
					
				
	(* applies the cfg's rules to the given word *)
	let rec subVar w vs rs = 
		match w with
			| [] -> Set.make [[]]
			| x::xs -> if (Set.belongs x vs) then subX x (subVar xs vs rs) rs
				else concatWords (Set.make [[x]]) (subVar xs vs rs) 
	
				
	(* removes the empty symbol from all non-empty words *)
	let removeEpsi w = List.filter (fun c -> c <> epsilon) w 
		
				
	(* filters out all words that have variables and cleans any unnecessary epsilon  *)		
	let cleanNonWords ws vs =
		let hasVar w = List.exists (fun c -> Set.belongs c vs) w in
		let ws = Set.filter (fun w -> not (hasVar w)) ws in
			Set.map (fun w -> removeEpsi w) ws
				
			
	
	class model (arg: t JSon.alternatives) =
		object(self) inherit Model.model arg (modelDesignation ())

			val representation: t =
				let j = JSon.from_alternatives arg in
					if j = `Null then
						JSon.get_representation arg
					else
						let alphabet = JSon.field_char_set j "alphabet" in
						let variables = JSon.field_char_set j "variables" in
						let initial = JSon.field_string j "initial" in
						let rules = JSon.field_string_list j "rules" in						
							{	 
								alphabet = alphabet;
								variables = variables;
								initial = initial.[0];
								rules = CFGSyntax.parse (Set.make rules);
							}

			initializer self#handleErrors	(* placement is crucial - after representation *)

			method representation =
				representation 
			
			method toJSon: json =
				let ruleToJSon {head=h; body=b} = 
					let aa = String.concat "" (List.map (fun x -> Util.charToString x) b) in
					let hh = Util.charToString h in
					`String (hh^" -> "^aa) in
			
				let rep = representation in
				`Assoc [
					("kind", `String self#kind);
					("description", `String self#description);
					("name", `String self#name);
					("alphabet", `List (List.map (fun s -> `String (Util.charToString s)) (Set.toList rep.alphabet)));
					("variables", `List (List.map (fun s -> `String (Util.charToString s)) (Set.toList rep.variables)));
					("initial", `String (Util.charToString rep.initial) );
					("rules", `List (List.map ruleToJSon (Set.toList rep.rules)));
					]
								
			method validate: unit = (
			
				let isIntersectionValid = (Set.inter representation.variables representation.alphabet) = Set.empty in
			
				let isInitialValid = Set.belongs representation.initial representation.variables in
				
				let areRuleHeadsValid = 
					let hs = Set.map (fun r -> r.head) representation.rules in
						Set.subset hs representation.variables 
				in
				 
				let areRuleBodiesValid =
					let bs = Set.map (fun r -> Set.make r.body) representation.rules in
					let allValidSymbs = Set.add epsilon (Set.union representation.alphabet representation.variables) in
					let res = Set.exists (fun b -> not (Set.subset b allValidSymbs)) bs in
						not res
				in
				
				if not isIntersectionValid then 
					Error.error self#name
            			"intersection between alphabet and variables is not empty" ()
            	;
				
				if not isInitialValid then
            		Error.error (Util.charToString representation.initial)
            			"initial does not belong to the set of all variables" ()
            	;
            	
            	if not areRuleHeadsValid then
            		Error.error self#name
            			"not all rule heads belong to the set of all variables" ()
            	;
            	
            	if not areRuleBodiesValid then
            		Error.error self#name
            			"not all rule bodies have valid characters" ()
            	)
			
			method tracing: unit = ()
			
			(* This method checks if the grammar is regular 
			*
			* @returns bool -> true if regular, false otherwise
			*)	
			method isRegular : bool = 
				
				let vs = representation.variables in
				let alp = representation.alphabet in
				
				let bs = Set.map (fun r -> r.body) representation.rules  in
					
				let isRightLinear bs =
					let isRightLinearX b = 
						match b with
							| [a] -> (Set.belongs a alp) || a = epsilon
							| [a; v] -> (Set.belongs a alp) && (Set.belongs v vs)
							| _ -> false
					in
						Set.for_all (fun b -> isRightLinearX b) bs
				in
					
				let isLeftLinear bs = 
					let rec isLeftLinearX b = 
						match b with
							| [a] -> (Set.belongs a alp) || a = epsilon
							| [v; a] -> (Set.belongs v vs) && (Set.belongs a alp)
							| _ -> false
					in
						Set.for_all (fun b -> isLeftLinearX b) bs
				in
				
					isRightLinear bs || isLeftLinear bs
					
			
			(* This method checks if the given word is accepted by the grammar 
			*
			* @param testWord -> word to be tested
			*
			* @returns bool -> true if it accepts the word, false otherwise
			*)	
			method accept (testWord:word) : bool = 			
			
				(* any word with a symbol not from the cfg alphabet will not be accepted 
				if not (Set.subset (Set.make testWord) representation.alphabet) then false else
				*)
				
				let vs = representation.variables in
				
				
				(* for word wa, get subword to the left of its first variable *)
				let rec getPrefix wa =
					match wa with
						| [] -> []
						| x::xs -> if Set.belongs x vs then [] else x::(getPrefix xs) 
				in
						
				(* for word wa, get subword to the rigth of its last variable *)
				let getSuffix wa =
					let rec getSuffixX wa sfx =
						match wa with
							| [] -> sfx
							| x::xs -> let auxSfx = sfx@[x] in 
										if Set.belongs x vs then getSuffixX xs []
											else getSuffixX xs auxSfx
					in
						getSuffixX wa []
				in
			
				let rec firstNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n > 0 then x::(firstNElements xs (n-1)) else [] 
				in
						
				let rec lastNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n < (List.length w) then lastNElements xs n else w  
				in
				
				(* a word can be discarded if its prefix does not match the leftmostmost part of word w *)
				let keepByPrefix genW testW =
					let pgw = getPrefix genW in
					let ptw = firstNElements testW (List.length pgw) in
						pgw = [] || pgw = ptw
				in
				
				
				(* a word can be discarded if its suffix does not match the rightmost part of word w *)
				let keepBySufix genW testW =
					let sgw = getSuffix genW in
					let stw = lastNElements testW (List.length sgw) in
						sgw = [] || sgw = stw
				in
			
				(* the word inst discarded only if it cant be discarded by neither its prefix nor its suffix *)
				let toKeep w tw = (w = [] && tw = [])  || (keepByPrefix w tw && keepBySufix w tw) in
			
				
				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in
				let l = List.length testWord in
				
				let nextGeneration ws =	
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
					let rws = Set.filter (fun w -> not (exceedsMaxLen w l alph)) subsWs in
					let rws = Set.map (fun w -> removeEpsi w) rws in
						Set.filter (fun w -> toKeep w testWord ) rws 
												
				in				
								
				let start = Set.make [[representation.initial]] in
				
				let res = Set.historicalFixedPoint nextGeneration start in
					Set.exists (fun x -> x = testWord ) res
					
				
			

			method acceptWithTracing (testWord:word) = 
			
			
			
				let vs = representation.variables in
				
				
				(* for word wa, get subword to the left of its first variable *)
				let rec getPrefix wa =
					match wa with
						| [] -> []
						| x::xs -> if Set.belongs x vs then [] else x::(getPrefix xs) 
				in
						
				(* for word wa, get subword to the rigth of its last variable *)
				let getSuffix wa =
					let rec getSuffixX wa sfx =
						match wa with
							| [] -> sfx
							| x::xs -> let auxSfx = sfx@[x] in 
										if Set.belongs x vs then getSuffixX xs []
											else getSuffixX xs auxSfx
					in
						getSuffixX wa []
				in
			
				let rec firstNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n > 0 then x::(firstNElements xs (n-1)) else [] 
				in
						
				let rec lastNElements w n =
					match w with
						| [] -> []
						| x::xs -> if n < (List.length w) then lastNElements xs n else w  
				in
				
				(* a word can be discarded if its prefix does not match the leftmostmost part of word w *)
				let keepByPrefix genW testW =
					let pgw = getPrefix genW in
					let ptw = firstNElements testW (List.length pgw) in
						pgw = [] || pgw = ptw
				in
				
				
				(* a word can be discarded if its suffix does not match the rightmost part of word w *)
				let keepBySufix genW testW =
					let sgw = getSuffix genW in
					let stw = lastNElements testW (List.length sgw) in
						sgw = [] || sgw = stw
				in
			
				(* the word inst discarded only if it cant be discarded by neither its prefix nor its suffix *)
				let toKeep w tw = (w = [] && tw = [])  || (keepByPrefix w tw && keepBySufix w tw) in
			
				
				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in
				let l = List.length testWord in
				
				let nextGeneration ws =	
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
					let rws = Set.filter (fun w -> not (exceedsMaxLen w l alph)) subsWs in
					let rws = Set.map (fun w -> removeEpsi w) rws in
						Set.filter (fun w -> toKeep w testWord ) rws 
												
				in	
				
				let start = Set.make [[representation.initial]] in
				
				let res = Set.historicalFixedPointTracing nextGeneration start in
				
				
				let trimRes l =
					match l with
					| [] -> []
					| x::xs -> if Set.belongs testWord x then xs
								else l
				in
				
				let res2 = List.rev (trimRes (List.rev res)) in
				
								
				let printWset ws = 
					print_string "[";
					Set.iter (fun w -> print_string (Util.wordToString w); print_string ";") ws;
					Util.println "]";
				in
				
					List.iter (fun ws -> printWset ws) res2
				
				
			
			
			(* This method generates all words up the the given lenght that belong to the grammars language 
			*
			* @ param lenght -> the max lenght of generated words
			*
			* @returns words -> the set of generated words 
			*)	
			method generate (length:int) : words =  
				
				let alph = representation.alphabet in
				let vs = representation.variables in
				let rs = representation.rules in
				
				
				let nextGeneration ws =
					let subsWs = Set.flatMap (fun w -> subVar w vs rs) ws in
						Set.filter (fun w -> not (exceedsMaxLen w length alph)) subsWs
				in
				
				let start = Set.make [[representation.initial]] in
				
				let res = Set.historicalFixedPoint nextGeneration start in
								
					cleanNonWords res vs
				
			(* This method converts the right-linear grammar to its automaton equivalent
			*
			* @pre - the grammar needs to be regular
			*
			* @returns FiniteAutomaton.model -> the equivalent finite automaton
			*)	
			method toFiniteAutomaton =
			
				let alp = representation.alphabet in
				let vrs = representation.variables in
				let toStr = Util.charToString in	
				
				(* This name will always be unique in the generated automaton *)
				let accSt = "AccSt" in
				
				let alphabet = alp in
				let states = Set.map (fun v -> toStr v) representation.variables in
				let allStates = Set.add accSt states in				
				let initialState = toStr representation.initial in				
				let acceptStates = Set.make [accSt] in
				
							
				
				let ruleToTrans rh rb =					
					match rb with 						
						| [s;v] when Set.belongs s alp && Set.belongs v vrs	-> Set.make [(toStr rh, s, toStr v)]
						
						| [v] when Set.belongs v vrs -> Set.make [(toStr rh, epsilon, toStr v)]
									
						| [s] when Set.belongs s alp -> Set.make [(toStr rh, s, accSt)]						
						
						| [e] when e = epsilon -> Set.make [(toStr rh, epsilon, accSt)]
						
						| _ -> Set.empty
				in 

				let transitions = Set.flatMap (fun r -> ruleToTrans r.head r.body) representation.rules in	
				
			
				let fa = {alphabet = alphabet; allStates = allStates; initialState = initialState; 
							transitions = transitions; acceptStates = acceptStates} in
							
					new FiniteAutomaton.model (Representation (fa)) 
					
				
			(* This method converts the right-linear grammar to its equivalent regular expression
			*
			* @pre - the grammar needs to be regular
			*
			* @returns FiniteAutomaton.model -> the equivalent regular expression
			*)		
			method toRegularExpression =
				
				let fa = self#toFiniteAutomaton in
				
					fa#toRegularExpression 	
			
				
			
		end

end

and (*module*) ContextFreeGrammarTests: sig
end
=
struct
	let active = false

	let test0 () =
		let m = new ContextFreeGrammar.model (File "test cfg/cfg_abc.json") in
		let j = m#toJSon in
			JSon.show j
			
	let testRegular () =
		let m = new ContextFreeGrammar.model (File "test cfg/cfg_abc.json") in
		let ws = m#isRegular in
			if ws then Util.println "is regular" else Util.println "is not regular"		
		
			
	let testAcc () =
		let m = new ContextFreeGrammar.model (File "test cfg/cfg_abc.json") in
		let ws = m#accept [] in
			if ws then Util.println "Word was accepted" else Util.println "Word was not accepted"
	
	
	let testTrace () =
		let m = new ContextFreeGrammar.model (File "test cfg/cfg_abc.json") in
			m#acceptWithTracing ['0';'1'] 
	
	let testGen () =
		let m = new ContextFreeGrammar.model (File "test cfg/cfg_abc.json") in
		let ws = m#generate 4 in
			Set.iter (fun w -> Util.printWord w) ws
	
	
	let testToGrammar() =
		let m = new RegularExpression.model (File "test regEx/re_simple.json") in
		let res = m#toRegularGrammar in
			JSon.show res#toJSon	
	
	let testToAutomaton () =
		let m = new ContextFreeGrammar.model (File "test cfg/cfg_abc.json") in
		let res = m#toFiniteAutomaton in
			JSon.show res#toJSon		
	
	let testToRe () =
		let m = new ContextFreeGrammar.model (File "test cfg/cfg_abc.json") in
		let res = m#toRegularExpression in
			JSon.show res#toJSon	
	
	
	let runAll =
		if active then (
			Util.header "ContextFreeGrammarTests";
			testTrace ()
		)

end		
				
	
	

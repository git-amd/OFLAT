(** OCaml-Flat - An OCaml library for FLAT concepts  
 *
 * @version v 0.2
 *
 * @author Joao Goncalves <jmg.goncalves@campus.fct.unl.pt>  [main author]
 * @author Rita Macedo <rp.macedo@campus.fct.unl.pt>  
 * @author Artur Miguel Dias <amd@fct.unl.pt>
 * @author Antonio Ravara <aravara@fct.unl.pt>
 *
 * LICENCE - As now this is a private project, but later it
 *            will be open-source.
*)


open OCamlFlatSupport 

(*#use "OCamlFlatSupport.ml";; *)
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
			method checkEnumeration : Enumeration.enum -> bool 
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
			method checkEnumeration (enum:Enumeration.enum) = 
						Set.for_all (fun w -> self#accept w) enum#representation.inside
					 && Set.for_all (fun w -> not (self#accept w)) enum#representation.outside

	end

end


(* --- Enumeration --- *)

and (*module*) Enumeration : sig

	type t = {
		inside : words;
		outside : words
	}	
	
	class enum :	
		
	  t JSon.alternatives -> string ->
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
		inside : words;
		outside : words
	}				
				
	class enum (arg: 'r JSon.alternatives ) (expectedKind: string) =
		object(self) inherit Entity.entity arg expectedKind

			val representation: t =
				let j = JSon.from_alternatives arg in
					if j = `Null then
						JSon.get_representation arg
					else
						let inside = JSon.field_string_set j "inside" in
						let outside = JSon.field_string_set j "outside" in
						{
							inside = Set.map Util.listFromString inside;
							outside = Set.map Util.listFromString outside
						}
			

		initializer self#handleErrors	(* placement is crucial - after representation *)	
			
		method representation =
				representation 
		method validate = ()
		method toJSon: json =
				`Assoc [
					("kind", `String self#kind);
					("description", `String self#description);
					("name", `String self#name);
					("inside", `List ([]));
					("outside", `List ([]))
				]
		method tracing = ()
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
			method errors : string list
			method handleErrors : unit
			method validate : unit
			method toJSon: json
			
			method tracing : unit
			
			method acceptBreadthFirst: word -> bool
			method accept : word -> bool
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

			method representation : t
			method checkEnumeration : Enumeration.enum -> bool 
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
					("alphabet", `List (List.map (fun c -> `String (String.make 1 c)) (Set.toList rep.alphabet)));
					("states", `List (List.map (fun s -> `String s) (Set.toList rep.allStates)));
					("initialState", `String rep.initialState);
					("transitions", `List (List.map (fun (a,b,c) ->
						`List [`String a; `String (String.make 1 b); `String c]) (Set.toList rep.transitions)));
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
					
	
			method toRegularExpression =
			
			(*
				let fa = self#toDeterministic in
				
				let fa2 = fa#cleanUselessStates in				
				
				let rep = fa2#representation in	
				
			*)	
			
			
			
			
			
				new RegularExpression.model (Representation (RegExpSyntax.parse "ab+c*"))
				
				
				
			
		end

end

and (*module*) FiniteAutomatonTests : sig
end
 =
struct
	let active = false

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
		let fa = new FiniteAutomaton.model (File "test automaton/fa_minimize2.json") in
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
			testReachable ();
			testProductive ();
			testClean ()
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
			method errors : string list
			method handleErrors : unit
			method validate : unit
			method toJSon: json

			method accept : word -> bool
			method generate : int -> words
			method tracing : unit
			
			method alphabet : symbols
			method quasiLanguage : words 

			method toFiniteAutomaton : FiniteAutomaton.model
			method checkEnumeration : Enumeration.enum -> bool

			val representation : t
			
		  end
end
 =
struct
	open RegExpSyntax
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

			
			method accept (w: word): bool = 
			
				let sz = List.length w in
				let lang = self#generate sz in
					Set.belongs w lang
				  
			
			
			method generate (length: int): words = 
														
				let rec lang rep ln = 
					match rep with 
						| Plus(l, r) -> Set.union (lang l ln) (lang r ln)	
						| Seq(l, r) ->  let left = lang l ln in
										let rigth w = lang r (ln - (List.length w)) in
										let conc w = Util.concatAll w (Set.toList (rigth w)) in
											Set.flatMap (fun lw -> Set.make (conc lw)) left											
						| Star r -> 														
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
								Set.add [] (starX exp ln)								
						| Symb(c) -> if ln > 0 then Set.make [[c]] else Set.empty
						| Empty -> Set.make [[]]
						| Zero -> Set.empty 
				in  
					lang representation length
			
			
			
				
				
			(* generates the alphabet of all symbols in the regular expression *)					
			method alphabet: symbols =						
							
				let rec alf rep = 
					match rep with 
						| RegExpSyntax.Plus(l, r) -> Set.union (alf l) (alf r) 
						| RegExpSyntax.Seq(l, r) -> Set.union (alf l) (alf r)
						| RegExpSyntax.Star(r) -> alf r
						| RegExpSyntax.Symb(c) -> Set.make [c]
						| RegExpSyntax.Empty -> Set.empty
						| RegExpSyntax.Zero -> Set.empty
				in
					alf representation
			
			(* generates the language of the regular expression for when klenne is always zero 	*)				
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
				
			
			method tracing: unit = ()

			method toFiniteAutomaton: FiniteAutomaton.model = 
				
				let getInit = fun (a,_,_,_,_) -> a in
				let getSts = fun (_,a,_,_,_) -> a in
				let getAccs = fun (_,_,a,_,_) -> a in
				let getTrns = fun (_,_,_,a,_) -> a in
				let getAlf = fun (_,_,_,_,a) -> a in
				
								
				let k = ref 0 in
				
				let genName () =
					let n = !k in 
					let () = k:= n + 1 in
						"newSt" ^ (string_of_int n) in
				
				
		 		let representation = RegExpSyntax.parse "aa+d" in 
				
				let rec compile rep = 
					match rep with 
						| Plus(l, r) -> let fa1 = compile l in
										let fa2 = compile r in
										let newStart = genName () in
										let newSts = Set.add newStart (Set.union (getSts fa1) (getSts fa2)) in
										let newAccSts = Set.union (getAccs fa1) (getAccs fa2) in
										let newTran1 = (newStart, epsilon, getInit fa1) in
										let newTran2 = (newStart, epsilon, getInit fa2) in
										let newTrans = Set.add newTran1 (Set.add newTran2 
											(Set.union (getTrns fa1) (getTrns fa2))) in
										let newAlf = Set.union (getAlf fa1) (getAlf fa2) in
										
											 (newStart, newSts, newAccSts, newTrans, newAlf)											 
											
						| Seq(l, r) ->	let fa1 = compile l in
										let fa2 = compile r in
										let ist = getInit fa1 in
										let sts = Set.union (getSts fa1) (getSts fa2) in
										let asts = getAccs fa2 in										
										let newTrns = Set.map (fun x -> (x, epsilon, getInit fa2) ) (getAccs fa1) in										
										let trns = Set.union newTrns (Set.union (getTrns fa1) (getTrns fa2)) in
										let alf = Set.union (getAlf fa1) (getAlf fa2) in
						
											(ist, sts, asts, trns, alf)
						
						| Star(r) ->  ("", Set.empty, Set.empty, Set.empty, Set.empty)	
						
						| Symb(c) -> let newStart = genName () in
									 let newAcc = genName () in
									 let newSts = Set.make [newStart; newAcc] in
									 let newTrn = Set.make [(newStart, c, newAcc)] in
									(newStart, newSts, Set.make [newStart], newTrn, Set.make [c])
									
						| Empty ->  let newStart = genName () in
									(newStart, Set.make [newStart], Set.make [newStart], Set.empty, Set.empty)	
									
						| Zero -> let newStart = genName () in
									(newStart, Set.make [newStart], Set.empty, Set.empty, Set.empty)	
				in
				
				let newFA = compile representation in
								
			
				new FiniteAutomaton.model (Representation {alphabet = getAlf newFA; allStates = getSts newFA; 
					initialState = getInit newFA; transitions = getTrns newFA; acceptStates = getAccs newFA} ) 
			
			

			
			
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
				
				
	
	let testAlphabet () =
		let re = new RegularExpression.model (File "test regEx/re_abc.json") in
			Util.println "alphabet: "; Util.printAlphabet (Set.toList (re#alphabet));
			Util.println ""
	
	let testQuasiLang () =
		let re = new RegularExpression.model (File "test regEx/re_abc.json") in
			let ws = re#quasiLanguage in
			Util.printWordList (Set.toList ws)
		
	let testAccept () =
		let m = Model.loadModel "test regEx/re_abc.json" in
			if m#accept ['a';'a'] then Util.println "word was accepted" else Util.println "word was not accepted" 
						
				
	let testGenerateLanguage () =
		let re = new RegularExpression.model (File "test regEx/re_abc.json") in
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
			
	let test1 () =
		let re = new RegularExpression.model (File "test regEx/re_abc.json") in
			let j = re#toJSon in
				JSon.show j

	let test2 () =
		let fa = new FiniteAutomaton.model (File "test Automaton/fa_abc.json") in
		let r = fa#toRegularExpression in
		let j = r#toJSon in
			JSon.show j

	let runAll =
		if active then (
			Util.header "RegularExpressionTests";
			testToFA ()
		)
end




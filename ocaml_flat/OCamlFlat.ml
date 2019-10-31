(* OCamlFlat.ml - AMD/2019 *)

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
			
			method tracing : unit
			method accept : word -> bool
			method accept2 : word -> bool
			method generate : int -> words
			method reachable : state -> states
			method productive : states

			method isDeterministic : bool
			method toDeterministic : FiniteAutomaton.model
			method getUsefulStates : states
			method getUselessStates : states
			method areAllStatesUseful: bool
			method cleanUselessStates: FiniteAutomaton.model
			method isMinimized : bool
			method minimize : FiniteAutomaton.model
			method toRegularExpression : RegularExpression.model

			method representation : t
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
	let fuseStates sts = String.concat "," sts 
	
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
					("alphabet", `List (List.map (fun c -> `String (String.make 1 c)) rep.alphabet));
					("states", `List (List.map (fun s -> `String s) rep.allStates));
					("initialState", `String rep.initialState);
					("transitions", `List (List.map (fun (a,b,c) ->
						`List [`String a; `String (String.make 1 b); `String c]) rep.transitions));
					("acceptStates", `List (List.map (fun s -> `String s) rep.acceptStates))
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
            	let alpha = representation.alphabet@[epsilon] in
				
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
			method accept2 (w: word): bool =
						    
                let rec acc cf t sta =
                    match cf with
                        [] -> false
                        |(st,[])::ls -> List.mem st sta || acc ls t sta
                        |(st,x::xs)::ls ->
                            let n = nextStates st x t in
                            let cfn = List.map (fun c -> (c,xs)) n in
                            let n2 = nextStates st epsilon t in
                            let cfn2 = List.map (fun c -> (c,x::xs)) n2 in
                                acc (ls@cfn@cfn2) t sta in
                acc [(representation.initialState,w)] representation.transitions representation.acceptStates
       

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
						
                let rec accept2X sts w t =
                    match w with
                        [] -> (Set.inter sts representation.acceptStates) <> []
                        |x::xs -> let nextSts = transition sts x t in
							nextSts <> Set.empty && accept2X nextSts xs t in
						
                let i = closeEmpty [representation.initialState] representation.transitions in
                    accept2X i w representation.transitions
			
			
			
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
			
				let hasAcceptState sts accSts = Set.exists (fun st -> Set.belongs st accSts) sts in
				let nxtNonEmptyTrns st ts = Set.filter (fun (a,b,_) -> a = st && b <> epsilon) ts in
			
				let rec gen n state transitions accSts =	
				
					let clsEmpty = (closeEmpty [state] transitions) in					
					if n = 0 then 
						if hasAcceptState clsEmpty accSts then [[]] else [] 
					
					else						
						let trnsSet = Set.flatMap (fun st -> nxtNonEmptyTrns st transitions ) clsEmpty in
						let genX sy st l = Util.addAll sy (gen (l-1) st transitions accSts) in
							Set.flatMap (fun (_,sy,st) -> genX sy st n) trnsSet 
				in
				gen length representation.initialState representation.transitions representation.acceptStates
            

            (** 
			* This method generates all states that are reachable from the given state. A state is reachable from s if there 
			* exists a word that starting on s will lead to that state (review definition of reachable from lecture a15 slide6)
			* 
			* @param s:state -> the given state 
			*
			* @returns states -> the set of all states reachable from s. 
			*)
            method reachable (s:state): states =
			
				let neighbourSts st t = transitionGet3 (Set.filter (fun (a,_,_) -> a = st) t) in
                let nextStates sts t = Set.flatMap (fun st -> neighbourSts st t) sts in
                let remain s t = Set.filter (fun (a,_,_) -> not (Set.belongs a s)) t in
				
                let rec reach visited s t = if visited = s then [] else Set.union s ( reach s (nextStates s t) (remain s t) ) in
					reach [] [s] representation.transitions
					
					

			(** 
			* This method generates all productive states. A state is productive if there exists a word that will lead said state 
			* to an acceptance state (review definition of productive from lecture a15 slide6)
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
			* This method verifies if the automaton is deterministic (review definition of deterministic from lecture a8 slide12)
			*
			* @returns bool -> true if automaton is deterministic, false otherwise
			*
			* Desc: For each state s, this method checks if there exists 2 or more transitions with the same symbol from any  
			* state belonging to closeempty of s, independently of the state which said transitions will lead to (review definition of closeempty 
			* from lecture a13 slide12). If there is no state for which this property is true, then the automaton is deterministic 
			*)
			method isDeterministic: bool =
			
				let trnsFromSt st ts = Set.filter (fun (st1,sy,_) -> st1 = st && sy <> epsilon) ts in
			
				let isStDeter st ts =
					let allSts = closeEmpty [st] ts in
					let allTs = Set.flatMap (fun st -> trnsFromSt st ts) allSts in
					let sys = transitionGet2 allTs in
						Set.size allTs = Set.size sys in
						
				let hasNondeterSt = Set.exists (fun st -> not (isStDeter st representation.transitions) ) 
										representation.allStates in
					not hasNondeterSt
				
			(** 
			* This method converts the non-deterministic automaton into its deterministic equivalent (review determinization algorithm 
			* from lecture a13)
			*
			* @returns FiniteAutomaton.model -> the new deterministic automaton
			*
			* Desc: This methods follows the studied algorithm in lecture a13, if the automaton to determinize is already deterministic,
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
				
				
				let r1 = closeEmpty [representation.initialState] representation.transitions in
				
				(* all transitions of the new deterministic automaton *)				
				let trnsD = rsToTs [r1] [r1] Set.empty representation.alphabet in
								
				let tds = Set.map (fun (a,b,c) -> (fuseStates a, b, fuseStates c)) trnsD in
						
				let newInitialState = fuseStates r1 in
				
				let stSet1 = Set.map (fun (a,_,_) -> a) trnsD in
				let stSet2 = Set.map (fun (_,_,c) -> c) trnsD in
				let stSet = Set.union stSet1 stSet2 in
				
				let isAccepState st = Set.belongs st representation.acceptStates in
				let hasAnAccepSt set = Set.exists (fun st -> isAccepState st ) set in
				let newAccStsSet = Set.filter (fun set -> hasAnAccepSt set) stSet in
				
				let newAllSts = Set.map (fun set -> fuseStates set) stSet in
				let newAccSts = Set.map (fun set -> fuseStates set) newAccStsSet in
				
								
				new FiniteAutomaton.model (Representation {alphabet = representation.alphabet; 
					allStates = newAllSts; initialState = newInitialState;	transitions = tds; acceptStates = newAccSts} )
				
			
			(** 
			* This method generates the set of all useful states (review definition of useful state from lecture a15 slide6)
			*
			* @returns states -> the set of all useful states
			*)
			method getUsefulStates: states =
				Set.inter self#productive (self#reachable representation.initialState)
				
				
			(** 
			* This method generates the set of all non useful states (review definition of useful state from lecture a15 slide6)
			*
			* @returns states -> the set of all non useful states
			*)
			method getUselessStates: states =
				Set.diff representation.allStates self#getUsefulStates
				
			
			
			(** 
			* This method verifies if all the automaton's states are useful (review definition of useful state from lecture a15 slide6)
			*
			* @returns bool -> true if all states of the automaton are useful, false otherwise
			*)
			method areAllStatesUseful: bool = 
			
				let fa = self#cleanUselessStates in
				let rep = fa#representation in
				let usfSts = self#getUsefulStates in
					usfSts = rep.allStates 
				
			
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
								
                let usfAlf = Set.map (fun (_,a,_) -> a) usfTrs in
				let newAccSts = Set.inter representation.acceptStates usfSts in
				let usfSts = Set.add representation.initialState usfSts in
				
				new FiniteAutomaton.model (Representation {alphabet = usfAlf; allStates = usfSts; 
					initialState = representation.initialState;	transitions = usfTrs; acceptStates = newAccSts} )
					
					
			(** 
			* This method verifies if the automaton is minimal (review definition of minimal automaton from lecture a15)
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
			* This method minimizes the automaton (review algorithm from lecture a15)
			*
			* @returns FiniteAutomaton.model -> the new minimal equivalent automaton 
			*
			* Desc: The given automaton is minimized according to the process described in lecture a15.   
			*)
			method minimize: FiniteAutomaton.model = 					
				
				let rep = self#representation in	
								
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
						[] -> []
						|x::xs -> Set.union (Set.combinations [x] xs) (halfCombs xs) in
				let halfTriang = halfCombs rep.allStates in				
				
				(* given set of equivalent states dicti, substitutes state st for its leftmost equivalent state according to dicti *)
				let rec translate st dicti = 
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in				
				
				(* the set of equivalent state pairs are those not present in the set of distinct state pairs *)
				let equiv = Set.filter ( fun (a,b) -> not (Set.belongs (a,b) dist) && 
														not (Set.belongs (b,a) dist) ) halfTriang in		
														
				let eq = Set.map (fun (a,b) -> b) equiv in
				let newSts = Set.diff rep.allStates eq in
				let newInitSt = translate rep.initialState equiv in
				let newAccSts = Set.inter rep.acceptStates newSts in
				let newTrans = Set.map (fun (x,y,z) -> (translate x equiv,y,translate z equiv) ) rep.transitions in
				
				
				new FiniteAutomaton.model (Representation {alphabet = rep.alphabet; allStates = newSts; 
					initialState = newInitSt; transitions = newTrans; acceptStates = newAccSts} ) 
							
	
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
		let m = Model.loadModel "fa_abc.json" in
			let j = m#toJSon in
				JSon.show j

	let test1 () =
		let fa = new FiniteAutomaton.model (File "fa_abcd.json") in
			let j = fa#toJSon in
				JSon.show j

	let test2 () =
  		let fa = new FiniteAutomaton.model (File "fa_abc3.json") in
			Util.println "generated words size 0:"; Util.printWordList (fa#generate 0 );
  		    Util.println "generated words size 1:"; Util.printWordList (fa#generate 1 );
  		    Util.println "generated words size 2:"; Util.printWordList (fa#generate 2 );
  		    Util.println "generated words size 3:"; Util.printWordList (fa#generate 100 )

    let test3 () =
      		let fa = new FiniteAutomaton.model (File "fa_abc3.json") in
			let start = fa#representation.initialState in
      		    Util.println "reachable states:"; Util.printStates (fa#reachable start ); Util.println " "

    let test4 () =
          		let fa = new FiniteAutomaton.model (File "fa_abc.json") in
          		    Util.println "productive states:"; Util.printStates (fa#productive ); Util.println " "

    let test5 () =
          		let fa = new FiniteAutomaton.model (File "fa_isDeter.json") in
				let fa2 = new FiniteAutomaton.model (File "fa_isDeter2.json") in
				let fa3 = new FiniteAutomaton.model (File "fa_abc.json") in
          		    if fa#isDeterministic then
          		        Util.println "automata is deterministic" else Util.println "automata is non-deterministic";
					if fa2#isDeterministic then
          		        Util.println "automata is deterministic" else Util.println "automata is non-deterministic";
					if fa3#isDeterministic then
          		        Util.println "automata is deterministic" else Util.println "automata is non-deterministic"


    let test6 () =
          		let fa = new FiniteAutomaton.model (File "fa_abc.json") in
          		   if fa#accept ['~'] then Util.println "word was accepted" else Util.println "word was not accepted" ;
          		   if fa#accept [] then Util.println "word was accepted" else Util.println "word was not accepted";
          		   if fa#accept ['a'] then Util.println "word was accepted" else Util.println "word was not accepted";
          		   if fa#accept ['a';'d'] then Util.println "word was accepted" else Util.println "word was not accepted";
          		   if fa#accept ['a';'b';'a';'d'] then Util.println "word was accepted" else Util.println "word was not accepted";
          		   if fa#accept ['c'] then Util.println "word was accepted" else Util.println "word was not accepted"

	let test7 () =
		let fa = new FiniteAutomaton.model (File "fa_abc.json") in
			let mfa = fa#toDeterministic in
			let j = mfa#toJSon in
				JSon.show j
			
	let test8 () =
		let fa = new FiniteAutomaton.model (File "fa_minimize2.json") in
			let mfa = fa#minimize in
			let j = mfa#toJSon in
				JSon.show j
			


	let runAll =
		if active then (
			Util.header "FiniteAutomatonTests";
			test8 () 
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
		let m = Model.loadModel "re_abc.json" in
			let j = m#toJSon in
				JSon.show j

	let test1 () =
		let re = new RegularExpression.model (File "re_abc.json") in
			let j = re#toJSon in
				JSon.show j

	let test2 () =
		let fa = new FiniteAutomaton.model (File "fa_abc.json") in
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












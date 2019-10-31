(* OCamlFlat.ml - AMD/2019 *)

open OCamlFlatSupport;;

(* use "OCamlFlatSupport.ml";; *)


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

				
				
				
			(* valida o automato carregado em ficheiro *)
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
            				let fromSt = transitionGet1 representation.transitions in
            				let sy = transitionGet2 representation.transitions in
            				let toSt = transitionGet3 representation.transitions in
            				let alpha = representation.alphabet@[epsilon] in
            				let cond = (Set.subset fromSt representation.allStates) &&
            				(Set.subset sy alpha) && (Set.subset toSt representation.allStates) in
            				if not cond then
            					    Error.error self#name
            					        "not all transitions are valid" ()
            			    )

							
			method tracing : unit = ()

			
			
			(* verifica se a palavra w e aceite pelo automato usando uma logica baseada em configuracoes
			 
				Comecamos com o par formado pelo estado inicial e pela palavra w. 
				Para cada par (estado, palavra) geramos os novos pares em que o primeiro 
				elemento sera um estado alcancavel do estado anterior a partir do primeiro simbolo da palavra restante,
				e o segundo elemento sera o resto da palavra sem o primeiro simbolo. 
				A funcao termina quando nao conseguimos gerar nenhum novo par ou quando geramos todos os pares em que 
				a palavra esta vazia, e dizemos que w e aceite se pelo menos um desses pares tiver um estado de aceitacao.			 
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
       

			(* verifica se a palavra w e aceite pelo automato com base em logica de conjuntos
			 
				Comecamos com o conjunto de todos os estados contituidos pelo inicial, e pelos alcancaveis por epsilon
				a partir do inicial. 
				Para cada simbolo da palavra w, o conjunto de estados anteriores e transformado no conjunto de todos os
				estados obtidos a partir de cada estado do conjunto anterior ao consumir o dado simbolo.
				Terminamos quando nao conseguimos gerar nenhum conjunto, ou quando consumimos todos os simbolos de w.
				A palavra w e aceite se ao consumir todos os seus simbolos, o ultimo conjunto gerado possui pelo menos
				um estado de aceitacao			 
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
			
			
			
			(* gera todas as palavras de comprimento length aceites pelo automato
			
				Comecando pelo estado inicial, para um dado estado obtemos as transicoes a partir 
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
            

            (* gera o conjunto de todos os estados alcancaveis por s - consultar slides da aula 15
			
				Comecando pelo conjunto formado unicamente pelo estado inicial, a partir de cada conjunto de estados 
				obtemos o conjunto de todos os estados vizinhos de pelo menos um membro do conjunto anterior.
				Terminamos quando nenhum novo estado for gerado, sendo que o resultado sera o conjunto de todos os 
				estados ate entao gerados (incluindo o inicial)
			
			*)
            method reachable (s:state): states =
			
				let neighbourSts st t = transitionGet3 (Set.filter (fun (a,_,_) -> a = st) t) in
                let nextStates sts t = Set.flatMap (fun st -> neighbourSts st t) sts in
                let remain s t = Set.filter (fun (a,_,_) -> not (Set.belongs a s)) t in
				
                let rec reach visited s t = if visited = s then [] else Set.union s ( reach s (nextStates s t) (remain s t) ) in
					reach [] [s] representation.transitions
					
					

			(* gera o conjunto de todos os estados produtivos - consultar slides da aula 15
			
				Para cada estado do automato, e gerado o conjunto de todos os estados por eles alcancaveis,
				um estado e considerado produtivo se pelo menos um dos seus estados gerados for de aceitacao
				
			*)
            method productive: states =
			
				let reachsAccSt st = Set.exists (fun s -> Set.belongs s representation.acceptStates ) (self#reachable st) in
					Set.filter (fun st -> reachsAccSt st) representation.allStates

			
			(* verifica se o automato e determinista - consultar slides da aula 8
			
				Para cada estado, e gerado o conjunto de todos os estados alcancaveis por transicoes epsilon a partir dele,
				o automato e determinista se para todos os conjunto de estados gerados, nao houver duas ou mais transicoes 
				para o mesmo simbolo a partir de qualquer um ou mais dos seus elementos  
				
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
				
			(* converte o automato nao determinista num automato determinista - consultar slides da aula 13
			
				Aplicamos o algoritmo de determinizacao de acordo com a explicao dada nos slides da aula 13.
				
			*)
			method toDeterministic: FiniteAutomaton.model = 
			
				let move sts sy ts = Set.flatMap (fun st -> nextStates st sy ts ) sts in	
				
				let newR oneR sy ts = 
					let nxtSts = move oneR sy ts in
					let clsempty = closeEmpty nxtSts ts in
					Set.union nxtSts clsempty in
					
					
				let rToTs r = 
					let nxtTrans = Set.map (fun sy -> (r,sy,newR r sy representation.transitions)) representation.alphabet in
					Set.filter (fun (_,_,z) -> not (z = Set.empty)) nxtTrans in
					
				let rec rsToTs stsD rD trnsD alph = 
					let nxtTs = Set.flatMap (fun stSet -> rToTs stSet ) rD in
					let nxtRs = Set.map (fun (_,_,z) -> z) nxtTs in
					let newRs = Set.filter (fun r -> not (Set.belongs r stsD)) nxtRs in
					if newRs = Set.empty then (Set.union trnsD nxtTs) else 
						rsToTs (Set.union newRs stsD) newRs (Set.union trnsD nxtTs) alph  in	
				
				let r1 = Set.add representation.initialState
						(closeEmpty [representation.initialState] representation.transitions) in
				
								
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
				
			
			(* obtem todos os estados do automato que sejam uteis 
			
				O resultado e igual a intersecao entre o conjunto de todos os estados produtivos, e o conjunto de todos
				os estados alcancaveis a partir do estado inicial 
				
			*)
			method getUsefulStates: states =
				Set.inter self#productive (self#reachable representation.initialState)
				
				
			(* obtem todos os estados do automato que nao sejam uteis 
			
				O resultado e igual a diferenca entre o conjunto de todos os estados uteis e o conjunto 
				de todos os estados do automato
				
			*)
			method getUselessStates: states =
				Set.diff representation.allStates self#getUsefulStates
				
			
			(* verifica se todos os estados do automato sao uteis 
			
				E verdade que todos os estados do automato sao uteis se o conjunto de todos os estados desse automato 
				for igual ao conjunto de todos os seus estados uteis 
				
			*)
			method areAllStatesUseful: bool = 
			
				let fa = self#cleanUselessStates in
				let rep = fa#representation in
				let usfSts = self#getUsefulStates in
					usfSts = rep.allStates 
				
			
			(* devolve um automato equivalente mas sem estados inuteis, sem transicoes que envolvam esses estados, e sem 
			   simbolos do alfabeto que so aparecam nessas transicoes 
			
				O novo automato e criado ao filtrar todos os seus estados que nao sejam uteis, ao filtrar todas
				as transicoes de ou para algum estado nao util, e ao filtrar do alfabeto todos os simbolos que so aparecam 
				em transicoes envolvendo estados nao uteis
				
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
					
					
			(* verifica se o automato e minimo - consultar slides da aula 15
			
				O automato e minimo se ao lhe aplicarmos o algoritmo de minimizacao obtemos 
				um automato identico ao original
				
			*)	
			method isMinimized: bool = 
			
				let fa = self#minimize in
				let rep = fa#representation in
					Set.size representation.allStates = Set.size rep.allStates
				
			
			(* converte o automato no seu automato minimo equivalente - consultar slides da aula 15
			
				Aplicamos o algoritmo de minimizacao de acordo com a explicao dada nos slides da aula 15.
				Antes de executarmos este metodo, devemos verificar se o automato nao tem estados nao uteis 
				e se ele e determinista
				
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
				
				
				(* given states a b c d generates (a,b) (a,c) (a,d) (b,c) (b,d) (c,d) *)
				let rec halfCombs sts = 
					match sts with 
						[] -> []
						|x::xs -> Set.union (Set.combinations [x] xs) (halfCombs xs) in
				let halfTriang = halfCombs rep.allStates in				
				
				(* substitutes state st for its leftmost equivalent state *)
				let rec translate st dicti = 
					match dicti with
						[] -> st
						|(eq1,eq2)::xs -> if eq2 = st then eq1 else translate st xs in				
				
				
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
	let active = true

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












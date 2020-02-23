(* OCamlFlat.ml - AMD/2019 *)
#use "topfind";;
#require "yojson";;

#use "OCamlFlatSupport.ml";;
#use "OCamlFlat.ml";;

(* Toplevel types *) 

type finiteAutomaton = {
		alphabet: char list;			
		allStates: state list;			
		initialState: state;		
		transitions: FiniteAutomaton.transition list;	
		acceptStates: state list	
	}
	
type regularExpression = string	

type contextFreeGrammar = {
		alphabet: char list;			
		variables: char list;	
		initial: char;
		rules: string list
	}
	
type exercise = {
		inside: string list ;
		outside: string list
	}	

(* Toplevel convertions *)
	
let fa_convertTo (fa: FiniteAutomaton.t ) =
	let alpha = Set.toList fa.alphabet in
	let states = Set.toList fa.allStates in
	let initial = fa.initialState in
	let trans = Set.toList fa.transitions in
	let accepts = Set.toList fa.acceptStates in
		{
			alphabet = alpha;
			allStates = states;
			initialState = initial;
			transitions = trans;
			acceptStates = accepts
		}
		
let fa_convertFrom (fa: finiteAutomaton) : FiniteAutomaton.t =
		{
			alphabet = Set.make fa.alphabet;
			allStates = Set.make fa.allStates;
			initialState = fa.initialState;
			transitions = Set.make fa.transitions;
			acceptStates = Set.make fa.acceptStates
		}
	
let re_convertTo re : regularExpression = RegExpSyntax.toString re

let re_convertFrom re = RegExpSyntax.parse re

let cfg_convertTo (cfg: ContextFreeGrammar.t ) =
	let alpha = Set.toList cfg.alphabet in
	let variables = Set.toList cfg.variables in
	let initial = cfg.initial in
	let rules = CFGSyntax.toStringList cfg.rules in
		{
			alphabet = alpha;			
			variables = variables;	
			initial = initial;
			rules = rules
		}
		
let cfg_convertFrom (cfg: contextFreeGrammar) : ContextFreeGrammar.t =
		{
			alphabet = Set.make cfg.alphabet;			
			variables = Set.make cfg.variables;	
			initial = cfg.initial;
			rules = CFGSyntax.parse (Set.make cfg.rules)
		}
	
	
let exer_convertTo (exer: Exercise.t) = 
	let inws = Set.map (fun w -> Util.stringFromList w) exer.inside in
	let outws = Set.map (fun w -> Util.stringFromList w) exer.outside in
		{
			inside = Set.toList inws;
			outside = Set.toList outws
		}
	
let exer_convertFrom exer : Exercise.t = 
	let inws = List.map (fun s -> Util.listFromString s) exer.inside in
	let outws = List.map (fun s -> Util.listFromString s) exer.outside in
		{
			problem = "";
			inside = Set.make inws;
			outside = Set.make outws
		}	
	
let exer_convertFailures ins outs =
	let ins = Set.map (fun w -> Util.stringFromList w) ins in
	let outs = Set.map (fun w -> Util.stringFromList w) outs in
		(Set.toList ins, Set.toList outs)
		

(* Automaton functions *)

let fa_load file = 
	let a = new FiniteAutomaton.model (File file) in
		fa_convertTo a#representation

let fa_accept fa w =
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let w = Util.stringToWord w in
		a#accept w 
		
let fa_traceAccept fa w = 
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let w = Util.stringToWord w in
		a#acceptWithTracing w  
	
		
			
let fa_generate fa l =
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let b = Set.map (fun w -> Util.stringFromList w) (a#generate l) in
		Set.toList b
			
let fa_reachable fa =	
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let start = fa.initialState in
		Set.toList (a#reachable start)		
	
let fa_productive fa =
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
        Set.toList (a#productive)			
		
let fa_clean fa =	
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let b = a#cleanUselessStates in
		fa_convertTo b#representation			

	
let fa_toDeter fa =						
	let a = new FiniteAutomaton.model (Representation fa) in
	let b = a#toDeterministic in
		fa_convertTo b#representation
		

let fa_isDeter fa =	
	let fa = fa_convertFrom fa in
    let a = new FiniteAutomaton.model (Representation fa) in
        a#isDeterministic 
				
				
let fa_minimize fa = 
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let b = a#minimize in
		fa_convertTo b#representation
		
		
let fa_toRegex fa =
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let b = a#toRegularExpression in
		re_convertTo b#representation


(* Regex functions *)

let re_load file = 
	let a = new RegularExpression.model (File file) in
		re_convertTo a#representation

let re_alphabet re =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
		Set.toList a#alphabet
		
let re_accept re w =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
	let w = Util.stringToWord w in
		a#accept w
		
let re_trace re w =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
	let w = Util.stringToWord w in
		a#allTrees w
		
				
let re_generate re l =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
	let b = Set.map (fun w -> Util.stringFromList w) (a#generate l) in
		Set.toList b

		
let re_simplify re =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
	let b =	a#simplify in
		re_convertTo b#representation

let re_toFA re =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
	let b =	a#toFiniteAutomaton in
		fa_convertTo b#representation
	

	
(* CFG functions *)

let cfg_load file = 
	let a = new ContextFreeGrammar.model (File file) in
		cfg_convertTo a#representation

		
let cfg_accept cfg w =
	let cfg = cfg_convertFrom cfg in
	let a = new ContextFreeGrammar.model (Representation cfg) in
	let w = Util.stringToWord w in
		a#accept w
		
let cfg_trace cfg w =
	let cfg = cfg_convertFrom cfg in
	let a = new ContextFreeGrammar.model (Representation cfg) in
	let w = Util.stringToWord w in
		a#acceptWithTracing w  
		
		
				
let cfg_generate cfg l =
	let cfg = cfg_convertFrom cfg in
	let a = new ContextFreeGrammar.model (Representation cfg) in
	let b = Set.map (fun w -> Util.stringFromList w) (a#generate l) in
		Set.toList b
		

let cfg_toFA cfg =
	let cfg = cfg_convertFrom cfg in
	let a = new ContextFreeGrammar.model (Representation cfg) in
	let b =	a#toFiniteAutomaton in
		fa_convertTo b#representation
		
			
let cfg_toRe cfg =
	let cfg = cfg_convertFrom cfg in
	let a = new ContextFreeGrammar.model (Representation cfg) in
	let b =	a#toRegularExpression in
		re_convertTo b#representation
	
		
(* Exercise functions *)	


let exer_load file =
	let e = new Exercise.exercise (File file) in
		exer_convertTo e#representation			
		
let exer_testFA exer fa =
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let exer = exer_convertFrom exer in
	let e = new Exercise.exercise (Representation exer) in
		a#checkExercise e

let exer_testFAFailures exer fa =
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let exer = exer_convertFrom exer in
	let e = new Exercise.exercise (Representation exer) in
	let (ins,outs) = a#checkExerciseFailures e in
		exer_convertFailures ins outs
		
let exer_testRe exer re =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
	let exer = exer_convertFrom exer in
	let e = new Exercise.exercise (Representation exer) in
		a#checkExercise e

let exer_testReFailures exer re =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
	let exer = exer_convertFrom exer in
	let e = new Exercise.exercise (Representation exer) in
	let (ins,outs) = a#checkExerciseFailures e in
		exer_convertFailures ins outs
	
let exer_testCfg exer cfg =
	let cfg = cfg_convertFrom cfg in
	let a = new ContextFreeGrammar.model (Representation cfg) in
	let exer = exer_convertFrom exer in
	let e = new Exercise.exercise (Representation exer) in
		a#checkExercise e

let exer_testCfgFailures exer cfg =
	let cfg = cfg_convertFrom cfg in
	let a = new ContextFreeGrammar.model (Representation cfg) in
	let exer = exer_convertFrom exer in
	let e = new Exercise.exercise (Representation exer) in
	let (ins,outs) = a#checkExerciseFailures e in
		exer_convertFailures ins outs		











		
		
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
	
type enumeration = {
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
	
let enum_convertTo (enum: Enumeration.t) = 
	let inws = Set.map (fun w -> Util.stringFromList w) enum.inside in
	let outws = Set.map (fun w -> Util.stringFromList w) enum.outside in
		{
			inside = Set.toList inws;
			outside = Set.toList outws
		}
	
let enum_convertFrom enum : Enumeration.t = 
	let inws = List.map (fun s -> Util.listFromString s) enum.inside in
	let outws = List.map (fun s -> Util.listFromString s) enum.outside in
		{
			problem = "";
			inside = Set.make inws;
			outside = Set.make outws
		}	
	
let enum_convertFailures ins outs =
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
		
		
(* Enumeration functions *)	


let enum_load file =
	let e = new Enumeration.enum (File file) in
		enum_convertTo e#representation			
		
let enum_testFA enum fa =
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let enum = enum_convertFrom enum in
	let e = new Enumeration.enum (Representation enum) in
		a#checkEnumeration e

let enum_testFAFailures enum fa =
	let fa = fa_convertFrom fa in
	let a = new FiniteAutomaton.model (Representation fa) in
	let enum = enum_convertFrom enum in
	let e = new Enumeration.enum (Representation enum) in
	let (ins,outs) = a#checkEnumerationFailures e in
		enum_convertFailures ins outs
		
let enum_testRe enum re =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
	let enum = enum_convertFrom enum in
	let e = new Enumeration.enum (Representation enum) in
		a#checkEnumeration e

let enum_testReFailures enum re =
	let re = re_convertFrom re in
	let a = new RegularExpression.model (Representation re) in
	let enum = enum_convertFrom enum in
	let e = new Enumeration.enum (Representation enum) in
	let (ins,outs) = a#checkEnumerationFailures e in
		enum_convertFailures ins outs
	
		











		
		
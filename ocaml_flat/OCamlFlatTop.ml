(* OCamlFlat.ml - AMD/2019 *)
#use "topfind";;
#require "yojson";;

#use "OCamlFlatSupport.ml";;
#use "OCamlFlat.ml";;


(* Module loading functions *)

let fa_load file = 
	let a = new FiniteAutomaton.model (File file) in
		a#representation
		
let re_load file = 
	let r = new RegularExpression.model (File file) in
		r#representation		
		
let enum_load file =
	let e = new Enumeration.enum (File file) in
		e#representation

(* Automaton functions *)

let fa_accep fa w =
	let a = new FiniteAutomaton.model (Representation fa) in
		a#accept w 
			
let fa_generate fa l =
	let a = new FiniteAutomaton.model (Representation fa) in
		a#generate l 
			
let fa_reachable fa =			
	let a = new FiniteAutomaton.model (Representation fa) in
	let start = fa.initialState in
		a#reachable start			
	
let fa_productive fa =			
	let a = new FiniteAutomaton.model (Representation fa) in
        a#productive			
		
let fa_clean fa =	
	let a = new FiniteAutomaton.model (Representation fa) in
	let b = a#cleanUselessStates in
		b#representation			

	
let fa_toDeter fa =						
	let a = new FiniteAutomaton.model (Representation fa) in
	let b = a#toDeterministic in
		b#representation
		

let fa_isDeter fa =			
    let a = new FiniteAutomaton.model (Representation fa) in
        a#isDeterministic 
				
				
let fa_minimize fa = 
	let a = new FiniteAutomaton.model (Representation fa) in
	let b = a#minimize in
		b#representation
		
		
let fa_toRegex fa =
	let a = new FiniteAutomaton.model (Representation fa) in
		a#representation


(* Regex functions *)

let re_alphabet re =
	let a = new RegularExpression.model (Representation re) in
		a#alphabet
		
let re_accept re w =
	let a = new RegularExpression.model (Representation re) in
		a#accept w
				
let re_generate re l =
	let a = new RegularExpression.model (Representation re) in
		a#generate l

		
let re_simplify re =
	let a = new RegularExpression.model (Representation re) in
		a#simplify


let re_toFA re =
	let a = new RegularExpression.model (Representation re) in
		a#toFiniteAutomaton

		
		
		
		
		
		
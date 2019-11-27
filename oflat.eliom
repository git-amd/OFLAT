(** OFLAT - A graphical interface for the OCaml-Flat library written using the Ocsigen framework
 * 
 * @version v 0.2
 * 
 * @author Rita Macedo <rp.macedo@campus.fct.unl.pt>  [main author]
 * @author Joao Goncalves <jmg.goncalves@campus.fct.unl.pt>
 * @author Artur Miguel Dias <amd@fct.unl.pt>
 * @author Antonio Ravara <aravara@fct.unl.pt>
 * 
 * LICENCE - As now this is a private project, but later it will be open source.
 *  
*)


[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
    open OCamlFlatSupport
    open OCamlFlat
    open Js_of_ocaml
    open CSS
]

module OFlat_app =
  Eliom_registration.App (
    struct
      let application_name = "oflat"
      let global_data_path = None
    end)

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let script_uri =
  Eliom_content.Html.D.make_uri
      ~absolute:false   (* We want local file *)
      ~service:(Eliom_service.static_dir ())
      ["painting2.js"]

let script_uri1 =
  Eliom_content.Html.D.make_uri
  (Eliom_service.extern
     ~prefix:"https://unpkg.com"
     ~path:["cytoscape";"dist"]
     ~meth:
       (Eliom_service.Get
          Eliom_parameter.(suffix (all_suffix "suff")))
     ())
    ["cytoscape.min.js"]

let script_uri5 =
  Eliom_content.Html.D.make_uri
  (Eliom_service.extern
     ~prefix:"https://cdnjs.cloudflare.com"
     ~path:["ajax"; "libs"; "lodash.js"; "4.17.10"]
     ~meth:
       (Eliom_service.Get
          Eliom_parameter.(suffix (all_suffix "suff")))
     ())
    ["lodash.js"]

  let script_uri7 =
  Eliom_content.Html.D.make_uri
  (Eliom_service.extern
     ~prefix:"https://cytoscape.org"
     ~path:["cytoscape.js-edgehandles"]
     ~meth:
       (Eliom_service.Get
          Eliom_parameter.(suffix (all_suffix "suff")))
     ())
    ["cytoscape-edgehandles.js"]

      let lincs_service =
  Eliom_content.Html.D.a
  (Eliom_service.extern
     ~prefix:"http://nova-lincs.di.fct.unl.pt"
     ~path:[""]
     ~meth:
       (Eliom_service.Get
          Eliom_parameter.(suffix (all_suffix "suff")))
     ())
    [div ~a:[a_id "footerButton"][txt "NovaLincs"]]
    [""]

  let factor_service =
  Eliom_content.Html.D.a
  (Eliom_service.extern
     ~prefix:"https://release.di.ubi.pt"
     ~path:["factor"]
     ~meth:
       (Eliom_service.Get
          Eliom_parameter.(suffix (all_suffix "suff")))
     ())
    [div ~a:[a_id "footerButton"][txt "Factor"]]
    ["index.html"]

  let tezos_service =
  Eliom_content.Html.D.a
  (Eliom_service.extern
     ~prefix:"https://tezos.com/"
     ~path:[""]
     ~meth:
       (Eliom_service.Get
          Eliom_parameter.(suffix (all_suffix "suff")))
     ())
    [div ~a:[a_id "footerButton"][txt "Fundação Tezos"]]
    [""]

[%%client

module JS = 
  struct 

    let global =
	    Js_of_ocaml.Js.Unsafe.global

    let eval s =
	    Js_of_ocaml.Js.Unsafe.eval_string s
    
    let exec s =
	    ignore (eval s)

    let console =
	    Js_of_ocaml.Firebug.console

    let string s =
	    Js_of_ocaml.Js.string s

      let log j =
	    ignore (console##log j)

    let alert j =
	    ignore (global##alert j)

  end 

module Graphics
  =
  struct

    let paintNode node color = 
        JS.exec ("paintNode('" ^ node ^ "', '"^ color ^"')")

    let paintNode1 node color = 
        JS.exec ("paintNode1('" ^ node ^ "', '"^ color ^"')")

    let createEdge (f, s, t) = 
        JS.exec ("makeEdge('" ^ f ^ "', '" ^ t ^ "', '" ^ (String.make 1 s) ^ "')")

    let createEdge1 (f, s, t) = 
        JS.exec ("makeEdge2('" ^ f ^ "', '" ^ t ^ "', '" ^ (String.make 1 s) ^ "')")

    let createNode node isFinal =
        JS.exec ("makeNode1('" ^ node ^ "', '" ^ string_of_bool (isFinal) ^ "')")

    let createNode1 node isFinal =
        JS.exec ("makeNode2('" ^ node ^ "', '" ^ string_of_bool (isFinal) ^ "')")
    
    let startGraph () =   
        JS.exec ("start()")
    
    let startGraph1 () =   
        JS.exec ("start2()")

    let destroyGraph () =   
        JS.exec ("destroy1()")

    let destroyGraph1 () =   
        JS.exec ("destroy2()")

    let resetStyle () = 
        JS.exec ("resetStyle()")

    let fit () =   
        JS.exec ("fit()")

    let getRandom() = 
        let test = Random.int 16777215 in
        Printf.sprintf "#%06x" test

  end


module rec FiniteAutomatonAnimation : sig
	type transition = state * symbol * state
	type transitions = transition set
	type t = FiniteAutomaton.t
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
			method accept : word -> bool
      method acceptBreadthFirst: word -> bool
			method generate : int -> words
      method generateUntil : int -> words
			method reachable : state -> states
			method productive : states

			method isDeterministic : bool
			method toDeterministic : FiniteAutomaton.model
      method toDeterministic1: FiniteAutomatonAnimation.model
			method getUsefulStates : states
			method getUselessStates : states
			method areAllStatesUseful: bool
			method cleanUselessStates: FiniteAutomaton.model
      method cleanUselessStates1: FiniteAutomatonAnimation.model
			method isMinimized : bool
			method minimize : FiniteAutomaton.model
      method minimize1 : FiniteAutomatonAnimation.model
      method equivalencePartition: states set
			method toRegularExpression : RegularExpression.model

			method representation : FiniteAutomaton.t

      val mutable position : int
      val mutable steps : state Set.t array

      method changeSentence: unit -> unit
      method inputEdges: unit
      method inputEdges1: unit
      method inputNodes : unit
      method inputNodes1: unit
      method next: unit
      method paint: OCamlFlat.state -> int -> bool -> unit
      method paintStates: int -> OCamlFlat.state OCamlFlatSupport.Set.t -> unit
      method accept3: word -> bool Lwt.t
      method startAccept: unit
      method back: unit
      method drawExample: unit
      method drawExample1: unit
      method initialNode: bool -> FiniteAutomatonAnimation.model
      method newNode: state -> bool -> FiniteAutomatonAnimation.model
      method newTransition: state * symbol * state -> FiniteAutomatonAnimation.model
      method productivePainting: unit
      method reachablePainting: unit
      method usefulPainting: unit
      method stringAsList1: string -> char list
      method changeTheTestingSentence: string -> unit
      method newSentence1: string
      method paintMinimization: string array -> unit
      method numberStates: int
      method numberTransitions: int
      method getColors:int
      method drawMinimize: string array -> int -> unit
      method inputNodesPainting: string array -> int -> unit

		  end
end
   = 
  struct

  type transition =
		state	(* state *)
	  * symbol	(* consumed input symbol *)
	  * state	(* next state *)

	type transitions = transition set

	type t = FiniteAutomaton.t

	let modelDesignation () = "finite automaton"	

  let productiveColor = "orange"
  let reachableColor = "yellow"
  let usefulColor = "purple"
  let stepState = "blue"
  let acceptState = "green"
  let wrongFinalState = "red"

  let paintProductive state =
        Graphics.paintNode state productiveColor

  let paintReachable state =
        Graphics.paintNode state reachableColor

  let paintUseful state =
        Graphics.paintNode state usefulColor

  let paintMinimization state color = 
        Graphics.paintNode state color
  
  let paintMinimized state color = 
        Graphics.paintNode1 state color

  let getMinStates list color = 
    List.iter (fun el -> paintMinimization el color) list

  let rec intersection l1 l2 =
     match l1 with
        [] -> []
      | x::xs -> (if List.mem x l2 then [x] else []) @ intersection xs l2

  let iterateList meth list =
  List.iter (fun el -> (meth el) ) list

  let cut s = (String.get s 0, String.sub s 1 ((String.length s)-1)) ;;
    
  let rec stringAsList s =
        if s = "" then []
        else
            let (x,xs) = cut s in
                x::stringAsList xs
    ;;

  let sentence: char list ref = ref []

  let newSentence = ref ""

  let rec delay n = if n = 0 then Lwt.return ()
                                  else Lwt.bind (Lwt.pause()) (fun () -> delay (n-1))

  class model (arg: FiniteAutomaton.t JSon.alternatives) =
		object(self) inherit FiniteAutomaton.model arg as super

      val mutable steps = [||]

      val mutable position = 0

      method changeSentence () = 
        newSentence := "";
        let bar = '|' in 
          if position == 0 then
            (newSentence:= !newSentence ^ String.make 1 bar;
            for i = 0 to (List.length !sentence) - 1 do 
              newSentence:= !newSentence ^ String.make 1 (List.nth !sentence i);
            done)
          else 
            (for i = 0 to position - 1 do
              newSentence:= !newSentence ^ String.make 1 (List.nth !sentence i);
            done;
            newSentence:= !newSentence ^ String.make 1 bar;
            for i = position to (List.length !sentence) - 1 do 
              newSentence := !newSentence ^ String.make 1 (List.nth !sentence i);
            done)

      method inputNodes  = 
        List.iter (fun el -> (Graphics.createNode el (List.mem el self#representation.acceptStates)) ) self#representation.allStates

      method inputNodes1  = 
        List.iter (fun el -> (Graphics.createNode1 el (List.mem el self#representation.acceptStates)) ) self#representation.allStates

      method inputNodesPainting colors number = 
        let listStates = self#representation.allStates in 
        for i=0 to number-1 do
          let newState = List.nth listStates i in 
          Graphics.createNode1 newState (List.mem newState self#representation.acceptStates);
          let color = Array.get colors i in
          paintMinimized newState color
        done
      

      method inputEdges  = List.iter (fun el -> (Graphics.createEdge el) ) self#representation.transitions

      method inputEdges1  = List.iter (fun el -> (Graphics.createEdge1 el) ) self#representation.transitions

      method paint state length final = 
            if (length != 0) then 
              Graphics.paintNode state stepState
            else 
              if (final) then
                Graphics.paintNode state acceptState
              else 
                Graphics.paintNode state wrongFinalState

      method paintStates length states = 
              Graphics.resetStyle();
              List.iter (fun el -> self#paint el length (List.mem el self#representation.acceptStates)) states

      method accept3 (w: word) =
        let nextEpsilon1 st t =
          let n = Set.filter (fun (a,b,c) -> st = a && b = epsilon) t in
                  Set.map ( fun (_,_,d) -> d ) n in			
				
				let rec nextEpsilons currsts t = 
					let ns = Set.flatMap (fun nst -> nextEpsilon1 nst t) currsts in
						if (Set.subset ns currsts) then ns else nextEpsilons (Set.union currsts ns) t in
						
        let nextStates st sy t =
          let n = Set.filter (fun (a,b,c) -> st = a && sy = b) t in
                  Set.map ( fun (_,_,d) -> d) n in
						
          let transition sts sy t = 
            let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
                      Set.union nsts (nextEpsilons nsts t) in
						
          let rec accept2X sts w t =
            match w with
              [] -> Lwt.bind (delay 100) (fun () -> Lwt.bind (Lwt.return (self#paintStates (List.length w) sts)) (fun () -> Lwt.return ((Set.inter sts self#representation.acceptStates) <> [])))
              |x::xs -> Lwt.bind (delay 50) (fun () -> Lwt.bind (Lwt.return (self#paintStates (List.length w) sts)) (fun () -> accept2X (transition sts x t) xs t)) in
						
          let i = Set.add self#representation.initialState
                  (nextEpsilons [self#representation.initialState] self#representation.transitions) in
                  accept2X i w self#representation.transitions

      method startAccept =
        steps <- Array.make 1000 [];
        position <- 0;
        
        let nextEpsilon1 st t =
          let n = Set.filter (fun (a,b,c) -> st = a && b = epsilon) t in
                  Set.map ( fun (_,_,d) -> d ) n in			
				
				let rec nextEpsilons currsts t = 
					let ns = Set.flatMap (fun nst -> nextEpsilon1 nst t) currsts in
						if (Set.subset ns currsts) then ns else nextEpsilons (Set.union currsts ns) t in

        let i = Set.add self#representation.initialState
                  (nextEpsilons [self#representation.initialState] self#representation.transitions) in
                  steps.(position) <- i;
                  self#paintStates ((List.length !sentence) - position) (Array.get steps position); self#changeSentence ();
                  
      method next =
        position <- position + 1;
        if position > List.length !sentence then
          (position <- position - 1; JS.alert ("A palavra terminou. Não existem mais estados seguintes."))
        else 
        (let nextEpsilon1 st t =
          let n = Set.filter (fun (a,b,c) -> st = a && b = epsilon) t in
                  Set.map ( fun (_,_,d) -> d ) n in			
				
				let rec nextEpsilons currsts t = 
					let ns = Set.flatMap (fun nst -> nextEpsilon1 nst t) currsts in
						if (Set.subset ns currsts) then ns else nextEpsilons (Set.union currsts ns) t in
						
        let nextStates st sy t =
          let n = Set.filter (fun (a,b,c) -> st = a && sy = b) t in
                  Set.map ( fun (_,_,d) -> d) n in
						
          let transition sts sy t = 
            let nsts = Set.flatMap (fun st -> nextStates st sy t) sts in
                      Set.union nsts (nextEpsilons nsts t) in
						
        steps.(position) <- (transition (Array.get steps (position-1)) (List.nth !sentence (position-1)) self#representation.transitions);
        self#paintStates ((List.length !sentence) - position) (Array.get steps position);
        self#changeSentence ())

      method back =
        position <- position - 1;
        if position < 0 then
          (position <- 0; JS.alert ("Não é possível andar para trás do estado inicial"))
        else 
          self#paintStates ((List.length !sentence) - position) (Array.get steps position); self#changeSentence ()

      method drawExample = 
        self#inputNodes;
        self#inputEdges

      method drawExample1 = 
        self#inputNodes1;
        self#inputEdges1
      
      method drawMinimize colors number =
        self#inputNodesPainting colors number;
        self#inputEdges1

      method initialNode final = 
      if final then
        (new FiniteAutomatonAnimation.model (Representation {
          alphabet = [];
	        allStates = ["START"]; 
          initialState = "START";
          transitions = [];
          acceptStates = ["START"]
        }))  
      else
        (new FiniteAutomatonAnimation.model (Representation {
          alphabet = [];
	        allStates = ["START"]; 
          initialState = "START";
          transitions = [];
          acceptStates = []
        }))
        

      method newNode  node final = 
        let rep: t = self#representation in 
          if final then
          new FiniteAutomatonAnimation.model (Representation{  
            alphabet = rep.alphabet;
	          allStates = rep.allStates @ [node];
            initialState = rep.initialState;
            transitions = rep.transitions;
            acceptStates = rep.acceptStates
            })
          else 
            new FiniteAutomatonAnimation.model (Representation{
            alphabet = rep.alphabet;
	          allStates = rep.allStates @ [node];
            initialState = rep.initialState;
            transitions = rep.transitions;
            acceptStates = rep.acceptStates @ [node]
            })

      method newTransition (a, b, c) = 
      let rep: t = self#representation in 
        new FiniteAutomatonAnimation.model (Representation{
            alphabet = rep.alphabet @ [b];
	          allStates = rep.allStates;
            initialState = rep.initialState;
            transitions = rep.transitions @ [(a, b , c)];
            acceptStates = rep.acceptStates
      })

      method getColors:int =
        List.length self#equivalencePartition

      method paintMinimization colors: unit = 
          let number = self#getColors in
          let listEquivalence = self#equivalencePartition in
          for i=0 to number-1 do 
            getMinStates (List.nth listEquivalence i) (Array.get colors i)
          done


      method productivePainting =
        let list1 = self#productive in
          let _ = JS.alert (List.length list1) in 
          let string_of_words l = String.concat ", " l in
            let blah = string_of_words list1 in
              let _ = JS.alert (blah) in 
        iterateList paintProductive list1

      method reachablePainting =
      iterateList paintReachable (self#reachable ("START"))

      method usefulPainting =
        let intre = intersection self#productive (self#reachable "START") in
        iterateList paintUseful intre 

      method stringAsList1 s = stringAsList s

      method changeTheTestingSentence word =
        sentence := stringAsList word

      method newSentence1 = !newSentence

      method minimize1: FiniteAutomatonAnimation.model = 			
        let blah = super#minimize in
          let rep = blah#representation in 
            new FiniteAutomatonAnimation.model (Representation rep) 
 
    method toDeterministic1: FiniteAutomatonAnimation.model = 
      let blah = super#toDeterministic in
        let rep = blah#representation in 
        new FiniteAutomatonAnimation.model (Representation rep) 


      method cleanUselessStates1: FiniteAutomatonAnimation.model =
        let blah = super#cleanUselessStates in
        let rep = blah#representation in 
        new FiniteAutomatonAnimation.model (Representation rep) 

      method numberStates: int =
        List.length self#representation.allStates

      method numberTransitions: int =
        List.length self#representation.transitions
							
    end

  end

module FiniteAutomatonExamples =
  struct
    let example1DFA = new FiniteAutomatonAnimation.model (Representation {
      alphabet = ['a'; 'b'; 'c'; 'd'];
	    allStates = ["START"; "A"; "AB"; "SUCCESS"]; 
      initialState = "START";
      transitions = [
            ("START",'a',"A"); ("START",'b',"START"); ("START",'c',"START");
                                                      ("START",'d',"START");
            ("A",'a',"A"); ("A",'b',"AB"); ("A",'c',"START"); ("A",'d',"START"); 
            ("AB",'a',"START"); ("AB",'b',"START"); ("AB",'c',"SUCCESS");
                                                    ("AB",'d',"START");
            ("SUCCESS",'a',"SUCCESS"); ("SUCCESS",'b',"SUCCESS");
                         ("SUCCESS",'c',"SUCCESS"); ("SUCCESS",'d',"SUCCESS")
        ];
      acceptStates = ["SUCCESS"]
    })

    let example2NFA = 
      let rep = example1DFA#representation in 
      new FiniteAutomatonAnimation.model (Representation {
      alphabet = rep.alphabet@ ['e'];
	    allStates = rep.allStates @ ["UNREACHABLE"; "UNPRODUCTIVE"];
      initialState = rep.initialState;
      transitions = rep.transitions @ [("A",'a',"AB"); ("UNREACHABLE", 'a', "SUCCESS"); 
                                               ("SUCCESS", 'e', "UNPRODUCTIVE"); ("UNPRODUCTIVE", 'a', "UNPRODUCTIVE")];
      acceptStates = rep.acceptStates
    })

    let example2DFA = new FiniteAutomatonAnimation.model (Representation {
      alphabet = ['a'; 'b'];
	    allStates = ["START"; "A"; "B"; "C"]; 
      initialState = "START";
      transitions = [("START", 'a', "A"); ("A", 'b', "B"); ("B", 'a', "C"); ("C", 'b', "B");
                   ("C", 'a', "A")
        ];
      acceptStates = ["START"; "B"; "C"]
    })

    let example3DFA = new FiniteAutomatonAnimation.model (Representation {
      alphabet = ['0'; '1'];
	    allStates = ["START"; "1"; "2"; "3"]; 
      initialState = "START";
      transitions = [("START", '1', "1"); ("1", '1', "START"); ("1", '0', "2"); ("2", '0', "1");
                   ("2", '1', "3"); ("3", '1', "2"); ("3", '0', "START"); ("START", '0', "3")
        ];
      acceptStates = ["1"]
    })

    let example1NFA = new FiniteAutomatonAnimation.model (Representation {
      alphabet = ['a'; 'b'];
	    allStates = ["START"; "A"; "B"]; 
      initialState = "START";
      transitions = [("START", 'a', "A"); ("A", 'b', "B"); ("A", 'b', "START"); ("B", 'a', "START")
        ];
      acceptStates = ["START"]
    })

  end 

module HtmlPageClient = 

  struct

let switch_visibility elt =
  let elt = Eliom_content.Html.To_dom.of_element elt in
  if Js_of_ocaml.Js.to_bool (elt##.classList##(contains (Js_of_ocaml.Js.string "hidden"))) then
    elt##.classList##remove (Js_of_ocaml.Js.string "hidden")
  else
    elt##.classList##add (Js_of_ocaml.Js.string "hidden")

end 

module Controller =
  struct

    let automata = ref (new FiniteAutomatonAnimation.model (Representation {
      alphabet = [];
	    allStates = ["START"]; 
      initialState = "START";
      transitions = [];
      acceptStates = []
    }))

    let automata1 = ref (new FiniteAutomatonAnimation.model (Representation {
      alphabet = [];
	    allStates = ["START"]; 
      initialState = "START";
      transitions = [];
      acceptStates = []
    }))

    let listColors = [|"Red"; "Yellow"; "Cyan"; "Green"; "Indigo"; "Blue"; "Magenta"; "Sienna"; "Violet"; "Orange"; "Lime"; "Teal"; "SteelBlue"; "Silver"; "Olive"; "Salmon"; "Crimson"; "Purple"; "DarkKhaki"; "PowderBlue"|]
    let listColorsBig: string array ref = ref [||]

    let getDeterminim () = 
      let isdeterministic = Dom_html.getElementById "isdeterministic" in
        if !automata#isDeterministic then 
          let deterministic = Js_of_ocaml.Js.string ("O autómato é determinista!") in 
          isdeterministic##.innerHTML := deterministic
        else 
          let deterministic = Js_of_ocaml.Js.string ("O autómato não é determinista!") in 
          isdeterministic##.innerHTML := deterministic
    
    let getminimism () = 
      let isminimal = Dom_html.getElementById "isminimal" in
          if !automata#isMinimized then 
          let minimal = Js_of_ocaml.Js.string ("O autómato é minimo!") in 
          isminimal##.innerHTML := minimal 
        else 
          let minimal = Js_of_ocaml.Js.string ("O autómato não é minimo!") in 
          isminimal##.innerHTML := minimal
    
    let gethasuselessStates () = 
      let isminimal = Dom_html.getElementById "areuseful" in
          if (!automata#areAllStatesUseful) then 
            (let minimal = Js_of_ocaml.Js.string ("O autómato não tem estados inúteis!") in 
            isminimal##.innerHTML := minimal)
          else 
          (let useless = !automata#getUselessStates in
            let number = List.length useless in 
              let string_of_words l = String.concat ", " l in
                let blah = string_of_words useless in
                let minimal = "O autómato tem " ^ (string_of_int number) ^ " estados inúteis: " ^ blah ^ "!" in 
                  isminimal##.innerHTML := Js_of_ocaml.Js.string minimal)

    let getNumberStates () = 
      let number = string_of_int !automata#numberStates in 
        let numberStates = Dom_html.getElementById "numberstates" in
          let sentence = "Número de Estados: " ^ number ^ "!" in 
          numberStates##.innerHTML := Js_of_ocaml.Js.string sentence
    
    let getNumberTransitions () = 
      let number = string_of_int !automata#numberTransitions in 
        let numberTrans = Dom_html.getElementById "numbertransitions" in
          let sentence = "Número de Transições: " ^ number ^ "!" in 
          numberTrans##.innerHTML := Js_of_ocaml.Js.string sentence
    
    let defineExample example =
      let cy = Dom_html.getElementById "cy" in
      cy##.style##.width:= Js_of_ocaml.Js.string "99%";
      let cy2 = Dom_html.getElementById "cy2" in
      cy2##.style##.width:= Js_of_ocaml.Js.string "0%";
      let info = Dom_html.getElementById "complete" in
        info##.style##.width:= Js_of_ocaml.Js.string "0%";
        info##.innerHTML := Js_of_ocaml.Js.string "";
      Graphics.destroyGraph();
      Graphics.startGraph();
      automata := example;
      !automata#drawExample;
      getDeterminim();
      getminimism();
      gethasuselessStates();
      getNumberStates();
      getNumberTransitions()

    let setColor () =
      let number = !automata#getColors in
      if (number <= 20) then 
        listColorsBig := listColors
      else 
        (for i=0 to 19 do 
          Array.set !listColorsBig i (Array.get listColors i)
        done;
        for i=20 to number-1 do
          let newColor = Graphics.getRandom() in 
          Array.set !listColorsBig i newColor
        done)
    
    let paintOriginal() =
      setColor();
      !automata#paintMinimization !listColorsBig

    let defineMinimized () =
      if (!automata#isDeterministic) then
        if (!automata#isMinimized) then 
          JS.alert ("O Autómato já é minimo")
        else 
          (let cy = Dom_html.getElementById "cy" in
          cy##.style##.width:= Js_of_ocaml.Js.string "49%";
          let cy2 = Dom_html.getElementById "cy2" in
          cy2##.style##.width:= Js_of_ocaml.Js.string "49%";
          let info = Dom_html.getElementById "complete" in
          info##.style##.width:= Js_of_ocaml.Js.string "0%";
          info##.innerHTML := Js_of_ocaml.Js.string "";
          Graphics.destroyGraph1();
          Graphics.startGraph1();
          Graphics.fit();
          automata1 := !automata#minimize1;
          paintOriginal();
          !automata1#drawMinimize !listColorsBig !automata#getColors)
      else 
        JS.alert ("O Autómato tem de ser determinista para poder ser minimizado")

    let getCompleteAutomaton () =
        let cy = Dom_html.getElementById "cy" in
        cy##.style##.width:= Js_of_ocaml.Js.string "49%";
        let cy2 = Dom_html.getElementById "cy2" in
        cy2##.style##.width:= Js_of_ocaml.Js.string "0%";
        Graphics.destroyGraph1();
        let info = Dom_html.getElementById "complete" in
        info##.style##.width:= Js_of_ocaml.Js.string "49%";
          let getInfo = JSon.to_string (!automata#toJSon) in
          info##.innerHTML := Js_of_ocaml.Js.string getInfo;
        Graphics.fit()

    let getDeterministic () =
      if (!automata#isDeterministic) then 
        JS.alert ("O Autómato já é determinista")
      else 
        (let cy = Dom_html.getElementById "cy" in
        cy##.style##.width:= Js_of_ocaml.Js.string "49%";
        let cy2 = Dom_html.getElementById "cy2" in
        cy2##.style##.width:= Js_of_ocaml.Js.string "49%";
        let info = Dom_html.getElementById "complete" in
          info##.style##.width:= Js_of_ocaml.Js.string "0%";
          info##.innerHTML := Js_of_ocaml.Js.string "";
        Graphics.destroyGraph1();
        Graphics.startGraph1();
        Graphics.fit();
        automata1 := !automata#toDeterministic1;
        !automata1#drawExample1)

    let createInitialNode isFinal = 
     automata := !automata#initialNode isFinal;
     Graphics.destroyGraph ();
     Graphics.startGraph ();
     Graphics.createNode "START" isFinal;
     getDeterminim();
      getminimism();
      gethasuselessStates();
      getNumberStates();
      getNumberTransitions()

    let createNode node isFinal = 
      ignore (automata := !automata#newNode node isFinal);
      Graphics.createNode node isFinal;
      getDeterminim();
      getminimism();
      gethasuselessStates();
      getNumberStates();
      getNumberTransitions()
    
    let createTransition (v1, c3, v2) =
      if (List.mem v1 !automata#representation.allStates) != true then
            JS.alert ("O estado de partida não existe!")
          else 
            if (List.mem v2 !automata#representation.allStates) != true then
              JS.alert ("O estado de chegada não existe!")
            else 
              ((ignore (automata := !automata#newTransition (v1, c3, v2)));
              Graphics.createEdge (v1, c3, v2);
              getDeterminim();
      getminimism();
      gethasuselessStates();
      getNumberStates();
      getNumberTransitions())

    let paintAllProductives () =
      Graphics.resetStyle();
      !automata#productivePainting
    
    let paintAllReachable () = 
      Graphics.resetStyle();
      !automata#reachablePainting

    let paintAllUseful () =
      Graphics.resetStyle();
      !automata#usefulPainting

    let accept word = 
        let w = !automata#stringAsList1 word in
        ignore (!automata#accept3 w)

    let startStep word =
      !automata#changeTheTestingSentence word;
      ignore (!automata#startAccept)

    let nextStep () =
      ignore (!automata#next)

    let backStep () = 
      ignore (!automata#back)

    let getNewSentence () = 
      Js_of_ocaml.Js.string !automata#newSentence1

    let createAutomataText texto = 
      let txt = Js_of_ocaml.Js.to_string texto in
      let fa = new FiniteAutomatonAnimation.model (Text txt) in
      defineExample fa
    
    let isDeterministic () =
      !automata#isDeterministic

    let getWords v = 
      !automata#generateUntil v

    let change () = 
      automata := !automata1;
      defineExample !automata

    let cleanUseless () =
      if (!automata#areAllStatesUseful) then 
        JS.alert ("O Autómato não tem estados para limpar, não existem estados inúteis!")
      else 
        (let cy = Dom_html.getElementById "cy" in
        cy##.style##.width:= Js_of_ocaml.Js.string "49%";
        let cy2 = Dom_html.getElementById "cy2" in
        cy2##.style##.width:= Js_of_ocaml.Js.string "49%";
        let info = Dom_html.getElementById "complete" in
          info##.style##.width:= Js_of_ocaml.Js.string "0%";
          info##.innerHTML := Js_of_ocaml.Js.string "";
        Graphics.destroyGraph1();
        Graphics.startGraph1();
        Graphics.fit();
        automata1 := !automata#cleanUselessStates1;
        !automata1#drawExample1)

    let closeCompleteAutomaton() =
      let cy = Dom_html.getElementById "cy" in
        cy##.style##.width:= Js_of_ocaml.Js.string "99%";
      let cy2 = Dom_html.getElementById "cy2" in
        cy2##.style##.width:= Js_of_ocaml.Js.string "0%";
      let info = Dom_html.getElementById "complete" in
        info##.style##.width:= Js_of_ocaml.Js.string "0%";
        info##.innerHTML := Js_of_ocaml.Js.string "";
        Graphics.fit()

    let printErrors () =
      let errors = !automata#errors in
      if errors = [] then 
        JS.alert "ok"
      else 
        JS.alert (String.concat "\n" errors)

  end


]

(** ------------------------------------------------------------------------
 * --- Local file reader widget --------------------------------------------
 * -------------------------------------------------------------------------
 *
 *   The main function is 'fileWidgetMake'. This function creates an
 *    <input type="file"> element supporting the interactive selection
 *    and loading of a local text file.
 *)

let%client fileWidgetCanceled () =
	JS.alert "Canceled"

let%client fileWidgetAction txt =
	Controller.createAutomataText txt;
  Controller.printErrors ()

let%client onFileLoad e =
	Js.Opt.case
		(e##.target)
		(fun () -> Js.bool false)
		(fun target ->
			Js.Opt.case
				(File.CoerceTo.string target##.result)
				(fun () -> Js.bool false)
				(fun data -> fileWidgetAction data; Js.bool false)
		)

let%client fileWidgetHandle filewidget =
	Js.Optdef.case
		(filewidget##.files)
		(fileWidgetCanceled)
		(fun files ->
			Js.Opt.case
				(files##item 0)
				(fileWidgetCanceled)
				(fun file -> 
					let reader = new%js File.fileReader in
						reader##.onload := Dom.handler onFileLoad;
						reader##readAsText (Js.Unsafe.coerce file)
		)		)

let%client fileWidgetEvents filewidget () =
	let fw = Eliom_content.Html.To_dom.of_input filewidget in
		Js_of_ocaml_lwt.Lwt_js_events.changes
			fw
			(fun _ _ -> fileWidgetHandle fw; Lwt.return ())

let fileWidgetMake () =
	let filewidget = input ~a: [a_id "file_input"; a_input_type `File] () in
	let _ = [%client (Lwt.async (fileWidgetEvents ~%filewidget): unit)] in
		filewidget

(* --------------------------------------------------------------------------
 * -------------------------------------------------------------------------- *)

module HtmlPage = 

  struct

  let hiddenBox2 box = div ~a:[a_id "examples"] [box]

  let mywidget s1 s2 name =
  let button1  = div ~a:[a_class ["button"]] [p ~a: [a_id name] [txt s1]] in
  let content = div ~a:[a_class ["content "; "hidden"]] [s2] in
  let _ = [%client
    (Lwt.async (fun () ->
       Js_of_ocaml_lwt.Lwt_js_events.clicks (Eliom_content.Html.To_dom.of_element ~%button1)
         (fun _ _ -> HtmlPageClient.switch_visibility ~%content; Lwt.return ()))
     : unit)
  ] in
  div ~a:[a_class ["mywidget"]] [button1; content]

  let upload =
  let onclick_handler5 = [%client (fun _ ->
    Controller.defineExample FiniteAutomatonExamples.example1DFA
  )] in
  let button5 = button ~a:[a_class ["text"]; a_id "invisible"; a_onclick onclick_handler5] [txt "Example 1"] in
  let onclick_handler6 = [%client (fun _ ->
    Controller.defineExample FiniteAutomatonExamples.example2DFA
  )] in
  let button6 = button ~a:[a_class ["text"]; a_id "invisible"; a_onclick onclick_handler6] [txt "Example 2"] in
  let onclick_handler7 = [%client (fun _ ->
    Controller.defineExample FiniteAutomatonExamples.example3DFA
  )] in
  let button7 = button ~a:[a_class ["text"]; a_id "invisible"; a_onclick onclick_handler7] [txt "Example 3"] in
  let onclick_handler8 = [%client (fun _ ->
    Controller.defineExample FiniteAutomatonExamples.example1NFA
  )] in
  let button8 = button ~a:[a_class ["text"]; a_id "invisible"; a_onclick onclick_handler8] [txt "Example 1"] in
  let onclick_handler9 = [%client (fun _ ->
    Controller.defineExample FiniteAutomatonExamples.example2NFA
  )] in
  let button9 = button ~a:[a_class ["text"]; a_id "invisible"; a_onclick onclick_handler9] [txt "Example 2"] in
  div ~a:[a_class ["box1"]] [div [div ~a:[a_class ["smallTitle"]] [txt "Exemplos de AFD"]; button5; button6; button7]; 
                            div [div ~a:[a_class ["smallTitle"]] [txt "Exemplos de AFN"]; button8; button9]]

let inputBox = input ~a:[a_id "box"; a_input_type `Text]()

let generate =

   let onclick_handler4 = [%client (fun _ ->
      Controller.createInitialNode false
  )] in 
  let button4 = button ~a:[a_onclick onclick_handler4] [txt "Adicionar estado inicial"] in
  let onclick_handler5 = [%client (fun _ ->
    Controller.createInitialNode true
  )] in 
  let button5 = button ~a:[a_onclick onclick_handler5] [txt "Adicionar estado inicial como final"] in
  let onclick_handler2 = [%client (fun _ ->
    let i = (Eliom_content.Html.To_dom.of_input ~%inputBox) in
    let v = Js_of_ocaml.Js.to_string i##.value in
      Controller.createNode v false
  )] in
  let button2 = button ~a:[a_onclick onclick_handler2] [txt "Adicionar estado"] in
  let onclick_handler6 = [%client (fun _ ->
    let i = (Eliom_content.Html.To_dom.of_input ~%inputBox) in
    let v = Js_of_ocaml.Js.to_string i##.value in
      Controller.createNode v true
  )] in
  let button6 = button ~a:[a_onclick onclick_handler6] [txt "Adicionar estado final"] in
  let onclick_handler3 = [%client (fun _ ->
    let i1 = (Eliom_content.Html.To_dom.of_input ~%inputBox) in
      let split = " " in
        let charSplit = String.get split 0 in 
      let v1 = Js_of_ocaml.Js.to_string i1##.value in
        let listOfThings = String.split_on_char charSplit v1 in 
        let c1 = List.nth listOfThings 0 in
        let c2 = List.nth listOfThings 2 in 
        let c3 = List.nth listOfThings 1 in
          let c4 = String.get c3 0 in
          Controller.createTransition (c1, c4, c2)
  )] in
  let button3 = button ~a:[a_onclick onclick_handler3] [txt "Adicionar Transição"] in
  div ~a:[a_class ["box1"]] [button4; button5; div ~a:[a_id "space"][button2; button6]; div [button3]]

  let evaluate = 
    let onclick_handler45 = [%client (fun _ ->
          Controller.paintAllProductives ()
    )] in
    let button45 = button ~a:[a_onclick onclick_handler45] [txt "Estados Produtivos"]
    in
    let onclick_handler46 = [%client (fun _ ->
          Controller.paintAllReachable ()
    )] in
    let button46 = button ~a:[a_onclick onclick_handler46] [txt "Estados Acessiveis"]
    in
    let onclick_handler47 = [%client (fun _ ->
          Controller.paintAllUseful ()
    )] in
    let button47 = button ~a:[a_onclick onclick_handler47] [txt "Estados Úteis"]
    in
    div ~a:[a_class ["box1"]] [button45; button46; button47]

let verify = 
      let onclick_handler11 = [%client (fun _ ->
        let i = (Eliom_content.Html.To_dom.of_input ~%inputBox) in
        let v = Js_of_ocaml.Js.to_string i##.value in
          Controller.accept v
      )         
      ] in
      let button11 = button ~a:[a_onclick onclick_handler11] [txt "Testar frase completa"] in
      let onclick_handler12 = [%client (fun _ ->
        let i = (Eliom_content.Html.To_dom.of_input ~%inputBox) in
        let v = Js_of_ocaml.Js.to_string i##.value in
        Controller.startStep v;
        let i0 = (Eliom_content.Html.To_dom.of_input ~%inputBox) in
        i0##.value:= Controller.getNewSentence()
      )         
      ] in
      let button12 = button ~a:[a_onclick onclick_handler12] [txt "Começar passo a passo"] in
      let onclick_handler13 = [%client (fun _ ->
        Controller.nextStep();
        let i0 = (Eliom_content.Html.To_dom.of_input ~%inputBox) in
        i0##.value:= Controller.getNewSentence()
      )         
      ] in
      let button13 = button ~a:[a_onclick onclick_handler13] [txt "Avançar"] in
      let onclick_handler14 = [%client (fun _ ->
        Controller.backStep();
        let i0 = (Eliom_content.Html.To_dom.of_input ~%inputBox) in
        i0##.value:= Controller.getNewSentence()
      )         
      ] in
      let button14 = button ~a:[a_onclick onclick_handler14] [txt "Retroceder"] in
      div ~a:[a_class ["box1"]] [txt "Palavra: "; button11; button12; button14; button13]

let answerBox = input ~a:[ a_input_type `Text]()

let zz = textarea ~a:[a_id "textarea"; a_rows 2; a_cols 20] (txt "")

let generateWords_handler = 
  [%client (fun _ ->
          let k = (Eliom_content.Html.To_dom.of_input ~%inputBox) in 
          let i = (Eliom_content.Html.To_dom.of_textarea ~%zz) in
          let v = Js_of_ocaml.Js.to_string k##.value in
            let size = int_of_string v in
          let test = Controller.getWords size in
            let string_of_word w = "' " ^ String.concat "" (List.map (String.make 1) w) ^ " '" in
            let string_of_words l = String.concat ", " (List.map string_of_word l) in 
              let res = string_of_words test in
                i##.value:= Js_of_ocaml.Js.string res
      )        
      ]

let words = button ~a:[a_onclick generateWords_handler] [txt "gerar palavras de tamanho x"]

let double_handler = 
  [%client (fun _ ->
        Controller.defineMinimized()
      )         
      ]

let double = button ~a:[a_onclick double_handler] [txt "Minimizar Autómato"]

let toDeterministic_handler = 
  [%client (fun _ ->
        Controller.getDeterministic()
      )         
      ]

let deter = button ~a:[a_onclick toDeterministic_handler] [txt "Determininizar Autómato"]

let toLeft_handler = 
  [%client (fun _ ->
        Controller.change ()
      )         
      ]

let toLeft = button ~a:[a_onclick toLeft_handler] [txt "Ficar com o Autómato Direito"]

let clean_handler = 
  [%client (fun _ ->
        Controller.cleanUseless()
      )         
      ]

let clean = button ~a:[a_onclick clean_handler] [txt "Limpar"]

let info_handler = 
  [%client (fun _ ->
        Controller.getCompleteAutomaton()
      )         
      ]

let completeInfo = button ~a:[a_onclick info_handler] [txt "Ver formatação do Autómato"]

let infoClose_handler = 
  [%client (fun _ ->
        Controller.closeCompleteAutomaton()
      )         
      ]

let closeInfo = button ~a:[a_onclick infoClose_handler] [txt "Fechar a formatação do Autómato"]


end





let () =
  OFlat_app.register
    ~service:main_service
    (fun () () ->
      let open Eliom_content.Html.D in
 
      Lwt.return
         (html
            (head (title (txt "Autómatos Animados")) [script ~a:[a_src script_uri1] (txt ""); 
                                                      script ~a:[a_src script_uri5] (txt "");
                                                      script ~a:[a_src script_uri7] (txt ""); 
                                                      css_link ~uri: (make_uri (Eliom_service.static_dir ()) ["codecss5.css"]) ();
                                                      script ~a:[a_src script_uri] (txt "");
                                                    ])
            (body [ div ~a:[a_class ["sidenav"]] [
                      div [h2 [txt "OFLAT"]; h3[txt "version 1.1"]];
                      HtmlPage.mywidget "Autómatos Finitos" (HtmlPage.hiddenBox2 (div [HtmlPage.inputBox;
                        HtmlPage.mywidget "Carregar Autómatos" (HtmlPage.hiddenBox2 (div [HtmlPage.upload; fileWidgetMake ()])) "under";
                        HtmlPage.mywidget "Gerar Autómatos" (HtmlPage.hiddenBox2 HtmlPage.generate) "under";
                        HtmlPage.mywidget "Testar aceitação de palavra" (HtmlPage.hiddenBox2 HtmlPage.verify) "under";
                        HtmlPage.mywidget "Avaliar Natureza dos Estados" (HtmlPage.hiddenBox2 HtmlPage.evaluate) "under";
                        div [HtmlPage.double];
                        div [HtmlPage.deter];
                        div [HtmlPage.toLeft];
                        div [HtmlPage.clean];
                        div [HtmlPage.words];
                        div [HtmlPage.completeInfo];
                        div [HtmlPage.closeInfo]])) "underBig";
                      p ~a: [a_id "underBig"][txt "Expressões Regulares"];
                      p ~a: [a_id "underBig"] [txt "Autómatos de Pilha"];
                      p ~a: [a_id "underBig"] [txt "Gramáticas Independentes de Contexto"]
                  ];
                    div ~a:[a_class ["main"]][
                        div ~a: [a_id "mainTitle"] [h1 [txt "Autómatos Animados"]];
                        div ~a: [a_class ["test"]][
                        div ~a:[a_id "cy"] [];
                        div ~a:[a_id "cy2"] [];
                        pre ~a:[a_id "complete"][];
                        div ~a:[a_id "infoBox"][
                          p ~a:[a_id "info"] [txt "Dados sobre o Autómato"];
                          p ~a:[a_id "isdeterministic"] [];
                          p ~a:[a_id "isminimal"] [];
                          p ~a:[a_id "areuseful"] [];
                          p ~a:[a_id "numbertransitions"] [];
                          p ~a:[a_id "numberstates"] [];
                          p ~a:[a_id "wordTitle"] [txt "Palavras geradas: "];
                          HtmlPage.zz];
                      ]
                    ];
					          footer ~a: [a_class ["footer"]] [txt "Desenvolvido em "; lincs_service; txt " dentro do projeto "; factor_service; txt  "/ Financiado por "; tezos_service]
                  ] 
            )
          )    
    )























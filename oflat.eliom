[%%shared
    open Eliom_lib
    open Eliom_content
    open Html.D
    open OCamlFlatSupport
    open OCamlFlat  
]

module Oflat_app =
  Eliom_registration.App (
    struct
      let application_name = "oflat"
      let global_data_path = None
    end)

let script_uri =
  Eliom_content.Html.D.make_uri
      ~absolute:false   (* We want local file *)
      ~service:(Eliom_service.static_dir ())
      ["code8.js"]

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

let script_uri2 =
  Eliom_content.Html.D.make_uri
      ~absolute:false   (* We want local file *)
      ~service:(Eliom_service.static_dir ())
      ["cytoscape-edgehandles"; "cytoscape-edgehandles.js"]

let script_uri3 =
  Eliom_content.Html.D.make_uri
  (Eliom_service.extern
     ~prefix:"https://unpkg.com"
     ~path:["popper.js@1.14.7";"dist"; "umd"]
     ~meth:
       (Eliom_service.Get
          Eliom_parameter.(suffix (all_suffix "suff")))
     ())
    ["popper.js"]

let script_uri4 =
  Eliom_content.Html.D.make_uri
      ~absolute:false   (* We want local file *)
      ~service:(Eliom_service.static_dir ())
      ["cytoscape-popper"; "cytoscape-popper.js"]

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

let script_uri6 =
  Eliom_content.Html.D.make_uri
  (Eliom_service.extern
     ~prefix:"https://unpkg.com"
     ~path:["tippy.js@4.0.1"; "umd"]
     ~meth:
       (Eliom_service.Get
          Eliom_parameter.(suffix (all_suffix "suff")))
     ())
    ["index.all.min.js"]

[%%client
let js_eval s =
	Js_of_ocaml.Js.Unsafe.eval_string s

let js_run s =
	ignore (js_eval s)

let js_console =
	Js_of_ocaml.Firebug.console

let js_string s =
	Js_of_ocaml.Js.string s

let js_log s =
	ignore (js_console##log (js_string s))


module FiniteAutomatonAnimation
   = 
  struct

  class model (arg: FiniteAutomaton.t JSon.alternatives) =
		object(self) inherit FiniteAutomaton.model arg

      method z = representation

      method createEdge (f, s, t) = 
        js_run ("makeEdge('" ^ f ^ "', '" ^ t ^ "', '" ^ (String.make 1 s) ^ "')")

      method createNode (node) =
          let test = List.mem node representation.acceptStates in 
            js_run ("makeNode1('" ^ node ^ "', '" ^ string_of_bool (test) ^ "')")

      method inputNodes  = List.iter (fun el -> (self#createNode el) ) representation.allStates

      method inputEdges  = List.iter (fun el -> (self#createEdge el) ) representation.transitions

    end

  end

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

let example2NFA = new FiniteAutomatonAnimation.model (Representation {
  alphabet = example1DFA#z.alphabet@ ['e'];
	  allStates = example1DFA#z.allStates @ ["UNREACHABLE"; "UNPRODUCTIVE"];
    initialState = example1DFA#z.initialState;
    transitions = example1DFA#z.transitions @ [("SUCCESS", 'a', "SUCCESS"); ("UNREACHABLE", 'a', "SUCCESS"); 
                                               ("SUCCESS", 'e', "UNPRODUCTIVE"); ("UNPRODUCTIVE", 'a', "UNPRODUCTIVE")];
    acceptStates = example1DFA#z.acceptStates
})

let initialAutomata = (new FiniteAutomatonAnimation.model (Representation {
    alphabet = [];
	  allStates = ["START"]; 
    initialState = "START";
    transitions = [];
    acceptStates = []
  }))

  let initialAutomataFinal = (new FiniteAutomatonAnimation.model (Representation {
    alphabet = [];
	  allStates = ["START"]; 
    initialState = "START";
    transitions = [];
    acceptStates = ["START"]
  }))

let automata = ref (new FiniteAutomatonAnimation.model (Representation {
    alphabet = [];
	  allStates = ["START"]; 
    initialState = "START";
    transitions = [];
    acceptStates = []
  }))

let drawExample (choice) = 
  js_run ("destroy1()");
  js_run ("start()");
  automata := choice;
  !automata#inputNodes;
  !automata#inputEdges

let newNode (node) = new FiniteAutomatonAnimation.model (Representation{
            alphabet = !automata#z.alphabet;
	          allStates = !automata#z.allStates @ [node];
            initialState = !automata#z.initialState;
            transitions = !automata#z.transitions;
            acceptStates = !automata#z.acceptStates
})

let newNodeFinal (node) = new FiniteAutomatonAnimation.model (Representation{
            alphabet = !automata#z.alphabet;
	          allStates = !automata#z.allStates @ [node];
            initialState = !automata#z.initialState;
            transitions = !automata#z.transitions;
            acceptStates = !automata#z.acceptStates @ [node]
})

let newTransition (a, b, c) = new FiniteAutomatonAnimation.model (Representation{
            alphabet = !automata#z.alphabet @ [b];
	          allStates = !automata#z.allStates;
            initialState = !automata#z.initialState;
            transitions = !automata#z.transitions @ [(a, b , c)];
            acceptStates = !automata#z.acceptStates
})

let rec intersection l1 l2 =
     match l1 with
        [] -> []
      | x::xs -> (if List.mem x l2 then [x] else []) @ intersection xs l2

let iterateList meth list =
  List.iter (fun el -> (meth el) ) list

let paintProductive fa =
  js_run ("paintProd('" ^ fa ^ "')")

let paintReachable fa =
  js_run ("paintReach('" ^ fa ^ "')")

let paintUseful fa = 
  js_run ("paintUseful('" ^ fa ^ "')")
]

let main_service =
  Eliom_service.create
    ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit)
    ()

let%client switch_visibility elt =
  let elt = Eliom_content.Html.To_dom.of_element elt in
  if Js_of_ocaml.Js.to_bool (elt##.classList##(contains (Js_of_ocaml.Js.string "hidden"))) then
    elt##.classList##remove (Js_of_ocaml.Js.string "hidden")
  else
    elt##.classList##add (Js_of_ocaml.Js.string "hidden")

let mywidget s1 s2 =
  let button1  = div ~a:[a_class ["button"]] [h3 ~a: [a_id "under"] [txt s1]] in
  let content = div ~a:[a_class ["content"; "hidden"]] [s2] in
  let _ = [%client
    (Lwt.async (fun () ->
       Lwt_js_events.clicks (Eliom_content.Html.To_dom.of_element ~%button1)
         (fun _ _ -> switch_visibility ~%content; Lwt.return ()))
     : unit)
  ] in
  div ~a:[a_class ["mywidget"]] [button1; content]

let hiddenBox2 box = div ~a:[a_id "examples"] [box]

let upload =
  let onclick_handler5 = [%client (fun _ ->
    drawExample (example1DFA)
  )] in
  let button5 = button ~a:[a_id "invisible"; a_onclick onclick_handler5] [txt "Example 1"] in
  let onclick_handler6 = [%client (fun _ ->
    drawExample (example2DFA)
  )] in
  let button6 = button ~a:[a_id "invisible"; a_onclick onclick_handler6] [txt "Example 1"] in
  let onclick_handler7 = [%client (fun _ ->
    drawExample (example3DFA)
  )] in
  let button7 = button ~a:[a_id "invisible"; a_onclick onclick_handler7] [txt "Example 2"] in
  let onclick_handler8 = [%client (fun _ ->
    drawExample (example1NFA)
  )] in
  let button8 = button ~a:[a_id "invisible"; a_onclick onclick_handler8] [txt "Example 2"] in
  let onclick_handler9 = [%client (fun _ ->
    drawExample (example2NFA)
  )] in
  let button9 = button ~a:[a_id "invisible"; a_onclick onclick_handler9] [txt "Example 3"] in
  div ~a:[a_id "examples"] [div ~a:[a_id "leftBox"] [txt "Exemplos de AFD"; button5; button6; button7]; 
                                           div ~a:[a_id "rightBox"] [txt "Exemplos de AFN"; button8; button9]]
let generate =
  let input1 = input ~a:[a_id "box"; a_input_type `Text]() in
  let input2 = input ~a:[a_id "box"; a_input_type `Text]() in
  let input3 = input ~a:[a_id "box"; a_input_type `Text]() in
  let input4 = input ~a:[a_id "box"; a_input_type `Text]() in
   let onclick_handler4 = [%client (fun _ ->
    automata := initialAutomata;
    js_run ("destroy1()");
    js_run ("start()");
    js_run ("makeNode1('" ^ "START" ^ "', '" ^ string_of_bool (false) ^ "')");
  )] in 
  let button4 = button ~a:[a_onclick onclick_handler4] [txt "Adicionar estado inicial"] in
  let onclick_handler5 = [%client (fun _ ->
    automata := initialAutomataFinal;
    js_run ("destroy1()");
    js_run ("start()");
    js_run ("makeNode1('" ^ "START" ^ "', '" ^ string_of_bool (true) ^ "')");
    automata := newNodeFinal "START"
  )] in 
  let button5 = button ~a:[a_onclick onclick_handler5] [txt "Adicionar estado inicial como final"] in
  let onclick_handler2 = [%client (fun _ ->
    let i = (Eliom_content.Html.To_dom.of_input ~%input1) in
    let v = Js_of_ocaml.Js.to_string i##.value in
      ignore (automata := newNode (v));
    js_run ("makeNode1('" ^ v ^ "', '" ^ string_of_bool (false) ^ "')")
  )] in
  let button2 = button ~a:[a_onclick onclick_handler2] [txt "Adicionar estado"] in
  let onclick_handler6 = [%client (fun _ ->
    let i = (Eliom_content.Html.To_dom.of_input ~%input1) in
    let v = Js_of_ocaml.Js.to_string i##.value in
      js_run ("makeNode1('" ^ v ^ "', '" ^ string_of_bool (true) ^ "')");
      ignore(automata := newNodeFinal (v))
  )] in
  let button6 = button ~a:[a_onclick onclick_handler6] [txt "Adicionar estado final"] in
  let onclick_handler3 = [%client (fun _ ->
    let i1 = (Eliom_content.Html.To_dom.of_input ~%input2) in
    let i2 = (Eliom_content.Html.To_dom.of_input ~%input3) in
    let i3 = (Eliom_content.Html.To_dom.of_input ~%input4) in
      let v1 = Js_of_ocaml.Js.to_string i1##.value in
      let v2 = Js_of_ocaml.Js.to_string i2##.value in
      let v3 = Js_of_ocaml.Js.to_string i3##.value in
        let c3 = String.get v3 0 in 
          ignore (automata := newTransition (v1, c3, v2));
          js_run ("makeEdge('" ^ v1 ^ "', '" ^ v2 ^ "', '" ^v3^"')");
  )] in
  let button3 = button ~a:[a_onclick onclick_handler3] [txt "Adicionar Transição"] in
  div ~a:[a_class ["mywidget"]] [button4; button5; div ~a:[a_id "space"][txt "Nome: ";input1; button2; button6]; div [txt "Estado de partida: "; input2; txt " Estado de chegada: "; input3; txt " Transição: "; input4; button3]]

  let evaluate = 
    let onclick_handler45 = [%client (fun _ ->
          js_run ("resetStyle()");
          iterateList paintProductive !automata#productive
    )] in
    let button45 = button ~a:[a_onclick onclick_handler45] [txt "Estados Produtivos"]
    in
    let onclick_handler46 = [%client (fun _ ->
          js_run ("resetStyle()");
          iterateList paintReachable (!automata#reachable ("START"))
    )] in
    let button46 = button ~a:[a_onclick onclick_handler46] [txt "Estados Acessiveis"]
    in
    let onclick_handler47 = [%client (fun _ ->
          js_run ("resetStyle()");
          let intre = intersection !automata#productive (!automata#reachable "START") in
          iterateList paintUseful intre 
    )] in
    let button47 = button ~a:[a_onclick onclick_handler47] [txt "Estados Úteis"]
    in
    div ~a:[a_class ["mywidget"]] [button45; button46; button47]

let () =
  Oflat_app.register
    ~service:main_service
    (fun () () ->
      let open Eliom_content.Html.D in
       Lwt.return
         (html
            (head (title (txt "Autómatos Animados")) [script ~a:[a_src script_uri1] (txt ""); 
                                        script ~a:[a_src script_uri3] (txt "");
                                        script ~a:[a_src script_uri5] (txt "");
                                        script ~a:[a_src script_uri4] (txt "");
                                        script ~a:[a_src script_uri6] (txt "");
                                          script ~a:[a_src script_uri2] (txt ""); 
                                          css_link ~uri: (make_uri (Eliom_service.static_dir ()) ["codecss2.css"]) ();
                                          script ~a:[a_src script_uri] (txt "");
                                          ])
            (body [ div [h1 [txt "Autómatos Animados"]];
                    div ~a:[a_id "inputBox"] 
                           [h2 [txt "Ações"];
                            mywidget "Carregar Autómatos" (hiddenBox2 upload);
                            mywidget "Gerar Autómatos" (hiddenBox2 generate);
                            mywidget "Avaliar Natureza dos Estados" (hiddenBox2 evaluate)
                           ];
                    div ~a:[a_id "cy"] [];
                  ] 
            )
          )    
    )  


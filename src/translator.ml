
open Graphe
open TopoSort

(* types de description des actions à exécuter *)

type unaire = Not
type binaire = Or | And | Nand | Xor
type ternaire = Mux

type expression =
    Const of bool
  | Unaire of unaire * int
  | Binaire of binaire * int * int
  | Ternaire of ternaire * int * int * int

type stmt =
    Assign of int * expression
  | Inputreg of int | Input of int
  | Outputreg of int | Output of int

type program = stmt list

(* Pour produire du Lics *)
let op_to_string = function
  | Or -> "or"
  | And -> "and"
  | Nand -> "nand"
  | Xor -> "xor"
    
let expr_to_string = function 
  | Const b -> string_of_bool b
  | Unaire(Not,i)-> Printf.sprintf "Not %d" i
  | Binaire(op,i1,i2) -> Printf.sprintf "%d %s %d" i1 (op_to_string op) i2
  | Ternaire(Mux,i1,i2,i3) -> Printf.sprintf "Mux(%d,%d,%d)" i1 i2 i3
    
let stmt_to_string = function
  | Assign(i1,exp) -> Printf.sprintf "%d = %s\n" i1 (expr_to_string exp)
  | Input(i)  -> Printf.sprintf  "%d = input\n" i
  | Output i -> Printf.sprintf "output %d\n" i
  | Inputreg i -> Printf.sprintf "%d = inputreg\n" i
  | Outputreg i-> Printf.sprintf "ouputreg %d\n" i

let lics_of_combin_graph g n = (* n = max_clé + 1 *)
  let sorted_keys = topoSort g.cgraph in
  (* lors du parcours de la liste, à tout noeud on associe la liste des
     variables qui sont ses pères *)
  let pred = Array.create n [] in
  let rec elimine = function
    | [] -> []
    | h::t ->
      let v = Vertex.getLabel (Graphe.find g.cgraph h) in
      if v = Noeud.Inreg || v = Noeud.Outreg
           || v = Noeud.Input || v = Noeud.Output 
      then elimine t
      else h:: elimine t in
  let translate k =
    let v = Graphe.find g.cgraph k in
    Vertex.iterSucc (function x -> pred.(x) <- k::pred.(x)) v ;
    match Vertex.getLabel v with
      | Noeud.Empty | Noeud.Reg ->
        failwith "lics_of_combin_graph: Noeud vide ou reg"
      | Noeud.True ->
        if pred.(k) = [] then Assign (k, Const true)
        else failwith "lics_of_combin_graph:
nombre d'entrées incorrect pour une constante"
      | Noeud.False ->
        if pred.(k) = [] then Assign (k, Const false)
        else failwith "lics_of_combin_graph:
nombre d'entrées incorrect pour une constante"
      | Noeud.Not ->
        begin
          try let [i] = pred.(k) in
              Assign (k, Unaire (Not, i))
          with Match_failure _ -> failwith
            "lics_of_combin_graph:
nombre d'entrées incorrect pour un opérateur unaire"
        end
      | Noeud.And | Noeud.Or | Noeud.Xor as op ->
        let tr_op = function
          | Noeud.And -> And
          | Noeud.Or -> Or
          | Noeud.Xor -> Xor
        in
        begin
          try let [i;j] = pred.(k) in
              Assign (k, Binaire (tr_op op, i, j))
          with Match_failure _ -> failwith
            "lics_of_combin_graph:
nombre d'entrées incorrect pour un opérateur unaire"
        end
      | Noeud.Mux (a,b,c) ->
        Assign (k, Ternaire (Mux, a, b, c))
      | _ -> failwith "lics_of_combin_graph: bad elimination"
  in
  let rec traite_cp typ = function (* typ = input ou output ou inreg ... *)
    | [] -> []
    | h::t ->
      let v = Graphe.find g.cgraph h in
      Vertex.iterSucc (function x -> pred.(x) <- h::pred.(x)) v ;
      (match typ with
        | "input" -> Input h
        | "inreg" -> Inputreg h
        | "output" ->
          begin
          try let [i] = pred.(h) in (* c'est l'entrée qui donne l'argument *)
              Output i
          with Match_failure _ -> failwith
            "lics_of_combin_graph:
nombre d'entrées incorrect pour un output"
          end
        | "outreg" ->
          begin
          try let [i] = pred.(h) in (* c'est l'entrée qui donne l'argument *)
              Outputreg i
          with Match_failure _ -> failwith
            "lics_of_combin_graph:
nombre d'entrées incorrect pour un outputreg"
          end
        | _ -> failwith "lics_of_combin_graph: traite_cp"
      ):: traite_cp typ t
  in
  traite_cp "input" g.cinputs @
    traite_cp "inreg" g.cinregs @
    List.map translate (elimine sorted_keys) @
    traite_cp "output" g.coutputs @
    traite_cp "outreg" g.coutregs

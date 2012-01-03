
open Graphe
open TopoSort
open LicsAst

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
      if v = Noeud.Inreg || v = Noeud.Input
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
          with Match_failure _ -> failwith (
            "lics_of_combin_graph:
nombre d'entrées (" ^ string_of_int (List.length pred.(k))  ^ ") incorrect pour " ^ (Noeud.string_of_label op) )
        end
      | Noeud.Mux ->
        try let [a;b;c] = pred.(k) in
            Assign (k, Ternaire (Mux, a, b, c))
        with Match_failure _ -> failwith (
            "lics_of_combin_graph:
nombre d'entrées (" ^ string_of_int (List.length pred.(k))  ^ ") incorrect pour un Mux" )
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
        | "output" -> Output h
        | "outreg" -> Outputreg h
        | _ -> failwith "lics_of_combin_graph: traite_cp"
      ):: traite_cp typ t
  in
  let l = traite_cp "input" g.cinputs in
  let l = l @ traite_cp "inreg" g.cinregs in
  let l = l @ List.map translate (elimine sorted_keys) in
  let l = l @ traite_cp "output" g.coutputs in
  let l = l @ traite_cp "outreg" g.coutregs in
  l


let licsBinFile_of_combin_graph g n s = (* s = nom du fichier à écrire *)
  LicsFileIO.write s (lics_of_combin_graph g n)

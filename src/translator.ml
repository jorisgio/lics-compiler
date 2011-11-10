
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

let lics_of_combin_graph g n = (* n = max_clé + 1 *)
  let sorted_keys = topoSort g.graph in
  (* lors du parcours de la liste, à tout noeud on associe la liste des
     variables qui sont ses pères *)
  let pred = Array.create n [] in
  let rec elimine = function
    | [] -> []
    | h::t -> if h = Graphe.Inreg 
              || h = Graphe.Outreg 
                || h = Graphe.Input 
                  || h = Graphe.Output 
      then elimine t
      else h:: elimine t in
  let translate k =
    let v = Graphe.find k g in
    Vertex.iterSucc (function x -> pred.(x) <- k) v ;
    match Vertex.getLabel v with
      | Graphe.Empty | Graphe.Reg ->
        failwith "lics_of_combin_graph: Noeud vide ou reg"
      | Graphe.True ->
        if pred.(k) = [] then Assign (k, Const (True))
        else failwith "lics_of_combin_graph:
nombre d'entrées incorrect pour une constante"
      | Graphe.False ->
        if pred.(k) = [] then Assign (k, Const (False))
        else failwith "lics_of_combin_graph:
nombre d'entrées incorrect pour une constante"
      | Graphe.Not ->
        begin
          try let [i] = pred.(k) in
              Assign (k, Unaire (Not, i))
          with Match_failure _ -> failwith
            "lics_of_combin_graph:
nombre d'entrées incorrect pour un opérateur unaire"
        end
      | Graphe.And | Graphe.Or | Graphe.Xor as op ->
        let tr_op = function
          | Graphe.And -> And
          | Graphe.Or -> Or
          | Graphe.Xor -> Xor
        in
        begin
          try let [i;j] = pred.(k) in
              Assign (k, Binaire (tr_op op, i, j))
          with Match_failure _ -> failwith
            "lics_of_combin_graph:
nombre d'entrées incorrect pour un opérateur unaire"
        end
      | Graphe.Mux (a,b,c) ->
        Assign (k, Ternaire (Mux, a, b, c))
      | _ -> failwith "lics_of_combin_graph: bad elimination"
  in
  let rec traite_cp typ = function (* typ = input ou output ou inreg ... *)
    | [] -> []
    | h::t ->
      let v = Graphe.find h g in
      Vertex.iterSucc (function x -> pred.(x) <- k) v ;
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
  traite_cp g.inputs "input" @
    traite_cp g.inregs "inreg" @
    List.map translate (elimine sorted_keys) @
    traite_cp g.outputs "output" @
    traite_cp g.outregs "outreg"


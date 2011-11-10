
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

let lics_of_combin_graphe g =
  let sorted_keys = topoSort g.graph in
  (* lors du parcours de la liste, à tout noeud on associe la liste des
     variables qui sont ses pères *)
  let pred = Hashtbl.create 43 in
  

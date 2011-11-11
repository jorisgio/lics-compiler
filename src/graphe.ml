exception Not_implemented

module Noeud = struct
	type t = int
	type label =
          | Empty
          | True | False
          | Reg | Input | Output | Inreg | Outreg
          | Not
          | And | Or | Xor | Nand
          | Mux of int * int * int
	let compare = Pervasives.compare
	let empty = Empty
end
module Graphe = Builder.MakeLabeledGraph(Noeud)
module Vertex = Graphe.Vertex

(*type renvoyé après la pemière passe (aka la construction du graphe) *)
type interm_graph = { igraph : Graphe.t; iinputs : Noeud.t list; ioutputs : Noeud.t list}

(* type renvoyé après la suppression des registres *)
type combin_graph = { cgraph : Graphe.t; cinputs : Noeud.t list; coutputs : Noeud.t list; cinregs : Noeud.t list; coutregs : Noeud.t list}
(* on suppose que les listes inputs et outputs / regs sont bien triées *)

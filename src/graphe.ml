
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
type label = Noeud.label
module Graphe = Builder.MakeLabeledGraph(Noeud)
module Vertex = Graphe.Vertex

(*type renvoyé après la pemière passe (aka la construction du graphe) *)
type interm_graph = { graph : Graphe.t; inputs : Noeud.t list; outputs : Noeud.t list}

(* type renvoyé après la suppression des registres *)
type combin_graph = { graph : Graphe.t; inputs : Noeud.t list; outputs : Noeud.t list; inregs : Noeud.t list; outregs : Noeud.t list}
(* on suppose que les listes inputs et outputs / regs sont bien triées *)

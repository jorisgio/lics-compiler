
module Noeud = struct
	type t = int
	type label = Empty | And | Or | Not | Xor | Reg | Input | Output | Inreg | Outreg |  True | False
	let compare = Pervasives.compare
	let empty = Empty
end
type label = Noeud.label
module Graphe = Builder.MakeLabeledGraph(Noued)
module Vertex = Graphe.Vertex


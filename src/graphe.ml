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
	let string_of_label = function
	  | True -> "True"
	  | False -> "False"
	  | Empty -> "Empty"
	  | Reg -> "Reg"
	  | Input -> "Input"
	  | Output -> "Output"
	  | Inreg -> "Inreg"
	  | Outreg -> "OUtreg"
	  | Not -> "Not"
	  | And -> "And"
	  | Or -> "Or"
	  | Xor -> "Xor"
	  | Nand -> "Nand"
	  | Mux(_,_,_) -> "Mux"
	  
end
module Graphe = struct
  include (Builder.MakeLabeledGraph(Noeud))
  
  (* print graph in f *)
  let drawGraph g f =
    let write key value =
      Printf.fprintf f "%d[label=%s];\n" key (Noeud.string_of_label (Vertex.getLabel value));
      let str= Vertex.foldSucc (fun  k acc-> acc ^ (Printf.sprintf "%d -> %d;\n" key k)) value "" in
      if String.length str > 0 then Printf.fprintf f "%s" str;
    in
    Printf.fprintf f "digraph circuit {\n" ;
    iterVertex write g;
    Printf.fprintf f "}"

end
module Vertex = Graphe.Vertex

(*type renvoyé après la pemière passe (aka la construction du graphe) *)
type interm_graph = { igraph : Graphe.t; iinputs : Noeud.t list; ioutputs : Noeud.t list}

(* type renvoyé après la suppression des registres *)
type combin_graph = { cgraph : Graphe.t; cinputs : Noeud.t list; coutputs : Noeud.t list; cinregs : Noeud.t list; coutregs : Noeud.t list}
(* on suppose que les listes inputs et outputs / regs sont bien triées *)


open Graphe

exception Circuit_combinatoire of Noeud.label

module Color = struct
  type t = Non_traite | Traitement_en_cours | Traite
  let empty = Non_traite
end

module ColoredGraph = Coloring.MakeColoredGraph(Graphe)(Color)

(* les noeuds sont en bleus quand ils sont traités *)
let topoSort graph = (* graph n'est pas coloré *)
  let rec traiter x v (l , graph) =
    (* l = accumulateur *)
    (* graph est coloré, représente le graphe après le parcours déjà effectué *)
    match ColoredGraph.getColor graph x with
      | Color.Traite -> (l , graph)
      | Color.Traitement_en_cours -> raise (Circuit_combinatoire (Graphe.getLabel (ColoredGraph.uncolorGraph graph) x) )
      | Color.Non_traite ->
        let l , graph = Graphe.Vertex.foldSucc (* traite ses fils *)
          (function x -> traiter x (ColoredGraph.find graph x))
          v
          (l , ColoredGraph.setColor graph x Color.Traitement_en_cours)
        in
        x::l , ColoredGraph.setColor graph x Color.Traite
  in
  fst (ColoredGraph.foldVertex
    traiter graph ([] , ColoredGraph.colorGraph graph))


open Graphe

module Color = struct
  type t = Bleu | Noir
  let empty = Noir
end

module ColoredGraph = Coloring.MakeColoredGraph(Graphe)(Color)

(* les noeuds sont en bleus quand ils sont traités *)
let topoSort graph = (* graphe n'est pas coloré *)
  let rec traiter x (l , graph) =
    (* l = accumulateur *)
    (* graph est coloré, représente le graphe après le parcours déjà effectué *)
    if ColoredGraph.getColor graph x = Color.Bleu then (l , graph)
    else (* si non traité *)
      let l , graph = Graphe.Vertex.foldSucc traiter (* traite ses fils *)
        (ColoredGraph.find graph x)
        (l , ColoredGraph.setColor graph x Color.Bleu) (* marquer le noeud *)
      in
      x::l , graph
  in
  fst (Graphe.foldVertex
    (fun x _ -> traiter x) graph ([] , ColoredGraph.colorGraph graph))


module Color = struct
  type t = Bleu | Noir
  let empty = Noir
end
module ColoredGraph = Coloring.MakeColoredGraph(Graphe.Graphe)(Color)

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
  fst (Graphe.Graphe.foldVertex
    (fun x _ -> traiter x) graph ([] , ColoredGraph.colorGraph graph))

(* Tests *)
(*

module Noeud = struct
  type t = int
  let compare = Pervasives.compare
  type label = int
  let empty = 0
end

module Couleur = struct
  type t = bool
  let empty = false
end



let rec construit taille acc count =
  match count with
  | k when k = taille -> acc
  | n -> construit taille (Graphe.Graphe.addVertex acc n ) (n+1)

let graphe = construit 5 ( Graphe.Graphe.empty ) 0

let graphe = Graphe.Graphe.addEdge graphe 1 2
let graphe = Graphe.Graphe.addEdge graphe 1 3
let graphe = Graphe.Graphe.addEdge graphe 2 3
let graphe = Graphe.Graphe.addEdge graphe 2 4
let graphe = Graphe.Graphe.addEdge graphe 3 4

let _ = topoSort graphe

*)

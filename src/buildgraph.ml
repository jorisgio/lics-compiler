(* Transformation du code en un graphe *)

open Ast

let Stbl = Hashtbl.Make(String)

(* type de la Hashtbl ou sont stockées les portes, à chaque identifiant, on associe une liste de porte :
   -un singleton pour une porte "initiale"
   -un elément pour chaque règle pour une définition inductive *)
type gtbl = gate list Stbl.t  

let gates_tbl = Stbl.create 42

(* parcours une liste de porte,  et ajoute les portes à la table gates_tbl *)
let rec parse_gates = 
  let rec addgate g = function
    | [] -> [g]
    | a::q when a.param = g.param -> a::q
    | a::q -> a::(addgate g q) in
  (function
    |[] -> ()
    | g::q when g.param = None -> Stbl.add gates_tbl g.name [g] 
    | g::q -> begin
      if Stbl.mem gates_tbl g.name then
	let li = Stbl.find  gates_tbl g.name in 
	Stbl.replace gates_tbl g.name (addgate g li)
      else
	Stbl.add gate_tbl g.name [g]
      )


(* gestion de l'induction *)

(* renvoit True si la cst est matchée par l'expression entière *)
let match_int cst epxr =
  true





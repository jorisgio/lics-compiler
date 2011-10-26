(* traduit les portes en graphes *)
open Ast

module Noeud = struct
    type t = int
    let compare = Pervasives.compare
end

module Graphe = Graph.MakeLabeled(Noeud)
module Vertex = Graphe.Vertex

exception Error of string

let unbound_var s = raise (Error ("unbound variable " ^ s))
let bad_arity x = raise (Error ("bad arity for " ^ x))

(* étiquettes du graphe *)
type label = lop | prefix

let rec process_lexpr graphe = function 
    | Bbinop (op, Bvar 

let rec l_stmt env = function   
    | Lassign (name, l_expr) -> 
    | _ -> raise (Error ("unknown statement in gate "))

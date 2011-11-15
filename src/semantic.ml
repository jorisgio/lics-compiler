(* Analyse sémantique de la syntaxe abstraite *)
open Ast

module Smap = Map.Make(String)
module I = struct
  type t = int
  let compare = Pervasives.compare
end
module Set = Set.Make(String)

(* définition des exceptions *)
exception UndefinedVar of string
    
(* vérifications sur les portes *)
  
(* vérifie que toutes les variables locales d'une porte 
   utilisées sont définies *)
let checkLocalDefined gate =
  let rec process_expr env =function
    | Bconst _ -> ()
    | Bvar ident when Set.mem ident env -> ()
    | Bvar ident -> raise (UndefinedVar ident)
    | Bbinop(_,e1,e2) -> process_expr env e1 ; process_expr env e2
    | Bprefix(_,e) -> process_expr env e
  in
  let process_stmt env =function
    | Lassign(ident, expr) -> process_expr env expr; Set.add ident env
    | _ -> raise Graphe.Not_implemented
  in
  let env =
    List.fold_left (fun env input -> Set.add input env) Set.empty gate.ginputs 
  in
  ignore (List.fold_left process_stmt env gate.gbody)
    
  
(* analyse le circuit *)
let analyse circuit = true
  

(* Analyse sémantique de la syntaxe abstraite *)
open Ast

module Smap = Map.Make(String)


(* définition des exceptions *)
exception UndefinedVar of string
    
(* vérifications sur les portes *)
  
(* vérifie que toutes les variables locales d'une porte 
   utilisées sont définies *)
let checkLocalDefined gate =
  let rec process_expr expr =
    | Bconst _ -> ()
    | Bvar ident when Set.mem ident env -> ()
    | Bvar ident -> raise UndefinedVar ident
    | Bbinop(_,e1,e2) -> process_expr e1 ; process_expr e2
    | Bprefix(_,e) -> process_expr e
  in
  let process_stmt env stmt =
    | Lassign(ident, expr) -> process_expr expr; Set.add ident env
    | _ -> raise Graphe.Not_implemented
  in
  let env =
    List.fold_left (fun env input -> Set.add input env) Set.empty gate.ginputs 
  in
  ignore (List.fold_left process_stmt env gate.gbody)
    
  
(* analyse le circuit *)
let analyse circuit = true
  

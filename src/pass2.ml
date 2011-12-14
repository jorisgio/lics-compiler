(* on parcour les blocs et on crée les arrêtes *)

open Ast
open Bast
open Graphe

module Noeud = struct
  include (Noeud)
    
  (* convertit un opérateur en étiquette *)
  let lop_to_label = function
    | BAst.And -> And 
    | BAst.Or -> Or 
    | BAst.Xor -> Xor 

  let lp_to_label   = function
    | BAst.Not -> Not
    | BAst.Reg -> Reg
      
  let bool_to_label = function
    | BAst.EBconst(true) -> True
    | BAst.EBconst(false) -> False
end

(* Traite une expression de type Bool récursivement en ajoutant les arrêtes aux graphe 
   Prend :
   gcur : graphe courant
   env : map qui à chaque variable associe un tableau des noeuds
   vertex : noeud auquel on doit lier l'expression 
   expr : l'expression
   Renvoit :
   le nouveau graphe *)
let processBExpr gcur env vertex expr = 

  let processRec gcur vertex = function
    | EBconst cst -> let cur = Smap.find (string_of_bool cst) env in
		     let gcur = Graphe.setLabel gcur cur.(0) (Noeud.bool_to_label cst) in
		     Graphe.addEdge gcur cur.(0) vertex

    | EVar ident -> let cur = Smap.find ident.id env in
		    Graphe.addEdge gcur cur.(0) vertex
    | EArray_i(ident,index) -> let cur = Smap.find ident.id env in
			       Graphe.addEdge gcur cur.(index) vertex
    | EPrefix(oper,exp) -> 
      let gcur = Graphe.addVertex2 gcur in
      let gcur = Graphe.setLabel2 gcur (Noeud.lp_to_label oper) in
      let cur = Graphe.curindex in
      let gcur = Graphe.addEdge gcur cur vertex in
      processRec gcur cur exp 
    | EInfix(oper, exp1, exp2) -> 
      let gcur = Graphe.addVertex2 gcur in
      let gcur = Graphe.setLabel2 gcur (Noeud.lop_to_label oper) in
      let cur = Graphe.curindex in
      let gcur = Graphe.addEdge gcur cur vertex in
      let gcur = processRec gcur cur exp1 in
      processRec gcur cur exp2
    | EMux(_,_,_) -> failwith "not implemented"

  in
  match expr.e with
    | EBconst cst -> Graphe.setLabel gcur vertex (Noeud.bool_to_label cst) 
    | EPrefix(oper, exp) -> 
      let gcur = Graphe.setLabel gcur vertex (Noeud.lp_to_label oper) in
      processRec gcur vertex exp 
    | EInfix(oper, exp1,exp2) -> 
      let gcur = Graphe.setLabel gcur vertex (Noeud.lop_to_label oper) in
      let gcur = processRec gcur vertex exp1 in
      processRec gcur vertex exp2
    | _ -> failwith "You cannot assign a left value to a left value"
      

(* TODO : traiter les tableaux comme des valeurs gauches *)
(* AJoute l'instruction au graphe.
   Prend :
   gcur : le graphe courant
   env : une map qui à chaque ident associe un tableau des noeuds du graphe
   instr : l'instruction
   Renvoit :
   le nouveau graphe 
*)
let processInstr gcur env = function
  | Assign(ident, exp) -> 
    let cur = Smap.find ident.id env in
    
    
    
    

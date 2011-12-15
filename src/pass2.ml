(* on parcour les blocs et on crée les arrêtes *)

open Ast
open Ast.Bast
open Graphe
(* open Semantic.Exceptions *)

exception Undefined
exception Error of pos * string
exception WrongType of pos * types * types
      
module Noeud = struct
  include (Noeud)
    
  (* convertit un opérateur en étiquette *)
  let lop_to_label = function
    | Bast.And -> And 
    | Bast.Or -> Or 
    | Bast.Xor -> Xor 

  let lp_to_label   = function
    | Bast.Not -> Not
    | Bast.Reg -> Reg
      
  let bool_to_label = function
    | Bast.EBconst(true) -> True
    | Bast.EBconst(false) -> False
end

(* Traite une expression de type Bool récursivement en ajoutant les arrêtes aux graphe 
   Prend :
   gcur : graphe courant
   env : map qui à chaque variable associe un tableau des noeuds
   vertex : noeud auquel on doit lier l'expression 
   expr : l'expression
   Renvoit :
   le nouveau graphe *)
let processBExpr gcur ind env vertex expr  =
  let index = ref ind in
  let rec  processRec gcur vertex expr = 
    match expr.e with
    | EBconst cst -> let cur = Smap.find (string_of_bool cst) env in
		     let gcur = Graphe.setLabel gcur cur.(0) (Noeud.bool_to_label (Sast.EBconst(cst))) in
		     Graphe.addEdge gcur cur.(0) vertex

    | EVar ident -> let cur = Smap.find ident.id env in
		    Graphe.addEdge gcur cur.(0) vertex
    | EArray_i(ident,index) -> let cur = Smap.find ident.id env in
			       Graphe.addEdge gcur cur.(index) vertex
    | EPrefix(oper,exp) -> 
      let gcur = incr index ; Graphe.addVertex gcur !index in
      let gcur = Graphe.setLabel gcur !index  (Noeud.lp_to_label oper) in
      let gcur = Graphe.addEdge gcur !index vertex in
      processRec gcur !index  exp 
    | EInfix(oper, exp1, exp2) -> 
      let gcur = incr index; Graphe.addVertex gcur !index in
      let gcur = Graphe.setLabel gcur !index (Noeud.lop_to_label oper) in
      let gcur = Graphe.addEdge gcur !index vertex in
      let gcur = processRec gcur !index exp1 in
      processRec gcur !index exp2
    | EMux(_,_,_) -> failwith "not implemented"

  in
  let g = 
    match expr.e with
      | EBconst cst -> Graphe.setLabel gcur vertex (Noeud.bool_to_label (Sast.EBconst(cst))) 
      | EPrefix(oper, exp) -> 
	let gcur = Graphe.setLabel gcur vertex (Noeud.lp_to_label oper) in
	processRec gcur vertex exp 
      | EInfix(oper, exp1,exp2) -> 
	let gcur = Graphe.setLabel gcur vertex (Noeud.lop_to_label oper) in
	let gcur = processRec gcur vertex exp1 in
	processRec gcur vertex exp2
      | _ -> failwith "You cannot assign a left value to a left value"
  in
  (g,!index)
      

(* TODO : traiter les tableaux comme des valeurs gauches *)
(* AJoute l'instruction au graphe.
   Prend :
   gcur : le graphe courant
   env : une map qui à chaque ident associe un tableau des noeuds du graphe
   instr : l'instruction
   Renvoit :
   le nouveau graphe 
*)
(* TODO : assign_i pour assigner  une valeur à un index d'un tableau  *)
let processInstr gcur index env = function
  | Assign(ident, exp) -> 
    let cur = Smap.find ident.id env in
    processBExpr gcur index env cur.(0) exp
  | _ -> failwith "Not implemented"

let processBlock gcur circuit blocks =
  let gate = Smap.find blocks.b_bgate_type circuit.b_gates in
  (* crée un tableau pour les entrées élémentaires du bloc *)
  let entrees_blocs = Array.make gate.ginputsize (-1) in
  let i = ref 0 in
  let rajoute_entree_bloc x = match x.e with
    | EArray_i (name,index) ->
        entrees_blocs.(!i) <-
          (Smap.find name.id circuit.b_blocsOutput).(index) ;
        incr i
    | EArray_r (name,min,max) ->
        for k = min to max do
          entrees_blocs.(!i) <-
            (Smap.find name.id circuit.b_blocsOutput).(k) ;
          incr i
        done
    | _ -> raise (Error (x.p,"mauvaise entrée")) 
  in
  begin
  try List.iter rajoute_entree_bloc blocks.b_binputs
  with Invalid_argument _ ->
    raise (Error ({line = 42; char_b = 42; char_e = 42},
    "Pas le bon nombre d'entrée pour le bloc " ^ blocks.b_bname))
  end;
  if !i < gate.ginputsize then
    raise (Error ({line = 42; char_b = 42; char_e = 42},
                  "Pas le bon nombre d'entrée pour le bloc "
                  ^ blocks.b_bname));
  i := 0 ;
  (* ajout à l'environnement b_bvertices de tableau de noeuds pour chaque entrée de la porte *)
  let ajoute_tab vertices entree = match entree.typ with
    | Bool -> let t = Array.make 1 entrees_blocs.(!i) in
      incr i ;
      Smap.add entree.id t vertices
    | Array n -> let t = Array.make n (-1) in
      for k = 0 to n - 1 do
        t.(k) <- entrees_blocs.(!i);
        incr i
      done;
      Smap.add entree.id t vertices
    | _ -> raise (WrongType ({line = 42; char_b = 42; char_e = 42},Int,Bool))
  in
  let index = (Graphe.size gcur) in
  let env =
    List.fold_left ajoute_tab blocks.b_bvertices gate.ginputs in
  (fst (List.fold_left
     (fun (gcur,index) instr -> processInstr gcur index env  instr.i)
     (gcur,index)
    gate.gbody))

let processFirstBlock gcur circuit blocks =
  let gcur = ref gcur in
  let inputs = ref [] in
  let gate = Smap.find blocks.b_bgate_type circuit.b_gates in
  (* crée un tableau pour les entrées élémentaires du bloc (crée les noeuds qui sont des Inputs) *)
  let entrees_blocs = Array.make gate.ginputsize (-1) in
  let i = ref 0 in
  let taille = ref (Graphe.size !gcur) in
  let rajoute_entree_bloc x = match x.e with
    | EArray_i (name,index) ->
      gcur := Graphe.addVertex !gcur !taille ;
      gcur := Graphe.setLabel !gcur !taille Noeud.Input;
      entrees_blocs.(!i) <- !taille;
      inputs := !taille :: !inputs;
      incr taille;
      incr i;
    | EArray_r (name,min,max) ->
        for k = min to max do
          gcur := Graphe.addVertex !gcur !taille ;
          gcur := Graphe.setLabel !gcur !taille Noeud.Input;
          entrees_blocs.(!i) <- !taille;
          incr taille;
          incr i;
        done
    | _ -> raise (Error (x.p,"mauvaise entrée")) 
  in
  begin
  try List.iter rajoute_entree_bloc blocks.b_binputs
  with Invalid_argument _ ->
    raise (Error ({line = 42; char_b = 42; char_e = 42},
    "Pas le bon nombre d'entrée pour le bloc " ^ blocks.b_bname))
  end;

  i := 0 ;
  (* ajout à l'environnement b_bvertices de tableau de noeuds pour chaque entrée de la porte *)
  let ajoute_tab vertices entree = match entree.typ with
    | Bool -> let t = Array.make 1 entrees_blocs.(!i) in
      incr i ;
      Smap.add entree.id t vertices
    | Array n -> let t = Array.make n (-1) in
      for k = 0 to n - 1 do
        t.(k) <- entrees_blocs.(!i);
        incr i
      done;
      Smap.add entree.id t vertices
    | _ -> raise (WrongType ({line = 42; char_b = 42; char_e = 42},Int,Bool))
  in
  let env =
    List.fold_left ajoute_tab blocks.b_bvertices gate.ginputs in
  (fst (List.fold_left
     (fun (gcur,index) instr -> processInstr gcur index env instr.i)
     (!gcur,!taille)
     gate.gbody) )
    ,
  !inputs

let process circuit =
  let hdAndTl_l = function
    | [] -> raise Not_found
    | h::t -> h,t
  in
  let rec last_l = function
    | [] -> raise Not_found
    | [a] -> a
    | h::t -> last_l t
  in
  let h,t =
    try
      hdAndTl_l circuit.b_blocks
    with Not_found -> failwith "Pas de blocs trouvés" in
  let cir, inputs = processFirstBlock circuit.b_graphe circuit h in
  let last_bloc = 
    try
      last_l circuit.b_blocks 
    with Not_found -> failwith "Pas de blocs trouvés" in
    { igraph =
        List.fold_left (fun gcur -> processBlock gcur circuit)
          cir
          t ;
      iinputs = inputs ;
      ioutputs = Array.to_list (Smap.find last_bloc.b_bname circuit.b_blocsOutput)
    }

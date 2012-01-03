(* on parcourt les blocs et on crée les arêtes *)

open Ast
open Ast.Bast
open Graphe

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
    | EBconst cst -> 
      assert (Smap.mem (string_of_bool cst) env) ;
      let cur = Smap.find (string_of_bool cst) env in
      let gcur = Graphe.setLabel gcur cur.(0) (Noeud.bool_to_label (Sast.EBconst(cst))) in
		     Graphe.addEdge gcur cur.(0) vertex

    | EVar ident ->
      assert (Smap.mem ident.id env) ;
      let cur = Smap.find ident.id env in
      Graphe.addEdge gcur cur.(0) vertex
    | EArray_i(ident,index) ->
      assert (Smap.mem ident.id env) ;
      let cur = Smap.find ident.id env in
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
    | EMux(exp1,exp2,exp3) ->
      let gcur = incr index; Graphe.addVertex gcur !index in
      let gcur = Graphe.setLabel gcur !index Noeud.Mux in
      let gcur = Graphe.addEdge gcur !index vertex in
	let gcur = processRec gcur !index exp1 in
	let gcur = processRec gcur !index exp2 in
	processRec gcur !index exp3

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
      | EMux(exp1,exp2,exp3) ->
	let gcur = Graphe.setLabel gcur vertex Noeud.Mux in
	let gcur = processRec gcur vertex exp1 in
	let gcur = processRec gcur vertex exp2 in
	processRec gcur vertex exp3
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
    assert (Smap.mem ident.id env) ;
    let cur = Smap.find ident.id env in
    processBExpr gcur index env cur.(0) exp
  | _ -> failwith "Not implemented"

let processBlock gcur circuit entrees_du_circuit blocks =
  let gate = Smap.find blocks.b_bgate_type circuit.b_gates in
  (* crée un tableau pour les entrées élémentaires du bloc *)
  let entrees_blocs = Array.make gate.ginputsize (-1) in
  let i = ref 0 in
  let rajoute_entree_bloc x = match x.e with
    | EVar id -> (* alors c'est nécessairement une entrée du circuit *)
      entrees_blocs.(!i) <- (
        try
          Smap.find id.id entrees_du_circuit
        with Not_found -> raise Undefined
      ).(0) ;
      incr i
    | EArray_i (name,index) ->
      entrees_blocs.(!i) <- (
      try
        Smap.find name.id circuit.b_blocsOutput
      with Not_found ->
        try
          Smap.find name.id entrees_du_circuit
        with Not_found -> raise Undefined
      ).(index) ;
      incr i
    | EArray_r (name,min,max) ->
      let arr =
        try
          Smap.find name.id circuit.b_blocsOutput
        with Not_found ->
          try
            Smap.find name.id entrees_du_circuit
          with Not_found -> raise Undefined in
      for k = min to max do
        entrees_blocs.(!i) <- arr.(k) ;
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


let process circuit =

(* crée un noeud par entrée et les ajoute dans une map, et dans une liste*)
let createInputNode (gcur,taille,env,acc) expr = match expr.e with
  | EVar { id = id ; typ = t} -> begin match t with
      | Bool ->
        Graphe.setLabel (Graphe.addVertex gcur taille) taille Noeud.Input ,
        taille + 1 ,
        Smap.add id (Array.make 1 taille) env,
        taille::acc
      | Array n ->
        let gcur = ref gcur in
        let taille = ref taille in
        let acc = ref acc in
        let arr = Array.make n (-1) in
        for k = 0 to n - 1 do
          gcur := Graphe.setLabel (Graphe.addVertex !gcur !taille) !taille Noeud.Input ;
          acc := !taille :: !acc ;
          arr.(k) <- !taille ;
          incr taille ;
        done;
        !gcur , !taille , Smap.add id arr env , !acc
  end
  | _ -> failwith "Wrong Type"
in

(* renvoit une liste d'index *)
let indexOfExpr expr acc =
  let acc = ref acc in
  begin
    match expr.e with
      | EVar(ident) -> begin
        try
          let ar = Smap.find ident.id circuit.b_blocsOutput in
          for i = (Array.length ar) - 1 downto 0 do 
            acc := ar.(i) :: !acc
          done
        with Not_found -> failwith "Variable de sortie du circuit non définie"
      end
      | EArray_i(id,i) -> begin
        try
          let ar = Smap.find id.id circuit.b_blocsOutput in
          acc := ar.(i) :: !acc 
        with Not_found -> failwith "Variable de sortie du circuit non définie"
      end
      | EArray_r(id,i1,i2) -> begin
        try
          let ar = Smap.find id.id circuit.b_blocsOutput in
          for i = i2 downto i1 do 
	    acc := ar.(i) :: !acc ;
          done
        with Not_found -> failwith "Variable de sortie du circuit non définie"
      end
  end;
  !acc
in

let gcur , _ , inp_map , inp =
  List.fold_left
    createInputNode
    (circuit.b_graphe,Graphe.size circuit.b_graphe,Smap.empty,[])
    circuit.b_inputs
in
  { igraph =
      List.fold_left (fun gcur -> processBlock gcur circuit inp_map)
        gcur
        circuit.b_blocks ;
    iinputs = inp ;
    ioutputs = List.fold_right indexOfExpr circuit.b_outputs []
  }

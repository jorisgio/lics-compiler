(* Transformation du code en un graphe *)

open Ast
open Graphe


(* chaines ordonées et hashables *)
module Str = struct
  include(String)
  let equal x y = ((Pervasives.compare x y) == 0)
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
end
module  Stbl = Hashtbl.Make(Str)

(* module implémentant l'environnement des variables locales *)
module Smap = Map.Make(String)

(* Workaround temporaire *)
let rec parse_gates gates_map q = match q with
  |[] -> gates_map
  | a::q -> parse_gates (Smap.add a.gname a gates_map) q

(* table des blocks *)
let block_tbl = Stbl.create 42 

(* on maintient la taille du graphe dans size *)
let size = ref 0
let __addVertex g = size :=  !size + 1 ; Graphe.addVertex g !size
let __setLabel g label = Graphe.setLabel g !size label

(* convertit un opérateur en étiquette, *TODO* *)
let lop_to_label = function
  | And -> Noeud.And 
  | Or -> Noeud.Or 
  | Xor -> Noeud.Xor 

let lp_to_label   = function
  | Not -> Noeud.Not

let bool_to_label = function
  | Cbool(true) -> Noeud.True
  | Cbool(false) -> Noeud.False

(* construit le graphe d'un circuit *)
let buildgraph cir = 
  (* on crée la table des portes *)
  let gates_map = parse_gates Smap.empty cir.gates in
  (* construit le nouveau graphe en ajoutant un block et renvoit ce graphe*)
  let rec add_block_to_graph intermcur b = 
    (* renvoit le couple du nouveau graphe * l'index du noeud de sortie après avoir traité l'expression *)
    let rec process_expr gcur env  = function
      | Bconst cst -> 
          let gcur = __addVertex gcur in
          let gcur = __setLabel gcur (bool_to_label cst) in
	  gcur,!size
      | Bvar ident -> 
          let src = Smap.find ident env in
          gcur,src
      | Bbinop(oper, expr1, expr2) -> 
          let cur = !size in
          let gcur = __addVertex gcur in 
          let gcur = __setLabel gcur (lop_to_label oper) in
          let gcur,i1 = process_expr gcur env  expr1 in
          let gcur,i2 = process_expr gcur env expr2 in
          let gcur = Graphe.addEdge gcur i1 cur in
          let gcur = Graphe.addEdge gcur i2 cur in
          gcur,cur
      | Bprefix(oper, expr) ->
          let cur = !size in
          let gcur = __addVertex gcur in
          let gcur = __setLabel gcur (lp_to_label oper) in
          let gcur,i = process_expr gcur env expr in
          let gcur = Graphe.addEdge gcur i cur in
          gcur,cur
    in
    (*renvoit le couple nouveau graphe * nouvel env après avoir traité le stmt *)
    let process_stmt (gcur,env) = function
      | Lassign(ident, expr) ->   
          let gcur,out = process_expr gcur env expr in
          let env = Smap.add ident out env in
      gcur,env
      | _ -> raise Not_implemented
    in
    (* on crée une environnement contenant les arguments formels de la porte liés aux blocks déjà instanciés *)
    
    let add_wire_to_env curenv ident wir =
      let varray = Stbl.find block_tbl wir.block_id in 
      (* le vecteur des noeuds de sortie du block *)
      Smap.add ident varray.(wir.out_id) curenv
    in
    (* on trouve la porte dont le block est une instance *)
    let gate = Smap.find b.bgate_type gates_map in
    (* on crée l'env *)
    let env = List.fold_left2 add_wire_to_env Smap.empty gate.ginputs b.binputs in
    (* on applique process_stmt sur tout les stmts de la porte 
       on obtient le nouveau graphe et un environnement *)
    let gcur,env = List.fold_left process_stmt (intermcur.igraph,env) gate.gbody in
    (* Workaround : maintenir la liste des sorties dans process_stmt ?*)
    (* on génère la liste des noeuds de sortie 
       et on ajoute notre block à la table des blocks *)
    let outputs = List.map (fun ident -> Smap.find ident env) gate.goutputs in
    Stbl.add block_tbl b.bname (Array.of_list outputs) ;
    { igraph = gcur ; ioutputs = outputs ; iinputs = intermcur.iinputs }
  in
  (* on traite le premier block du circuit *)
  let addToGraph (curgraph,li) _ =
    let li = (!size+1)::li in
    let curgraph = __addVertex curgraph in
    let curgraph = __setLabel curgraph Noeud.Input in
    curgraph,li 
  in
  let g,inputs =  List.fold_left addToGraph (Graphe.empty,[]) (cir.start).binputs in
  Stbl.add block_tbl cir.start.bname (Array.of_list inputs);
  List.fold_left add_block_to_graph {igraph= g; iinputs = inputs; ioutputs = [] } cir.blocks

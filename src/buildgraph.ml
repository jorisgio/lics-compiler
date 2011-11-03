(* Transformation du code en un graphe *)

open Ast

type interm = { graph : Graph.t; inputs : Vertex.t list; outputs : Vertex.t list} 
let Stbl = Hashtbl.Make(String)

(* module implémentant l'environnement des variables locales *)
module Smap = Map.Make(String)


(* parcours une liste de porte,  et ajoute les portes à la Map des tables *)
let rec parse_gates gates_map = 
  let rec addgate g = function
    | [] -> [g]
    | a::q when a.param = g.param -> a::q
    | a::q -> a::(addgate g q) in
  (function
    |[] -> gates_map
    | g::q when g.param = None -> Smap.add g.name [g] gates_map
    | g::q -> begin
      if Smap.mem g.name gates_map then
	let li = Smap.find g.name gates_map in 
	Smap.add g.name (addgate g li) gates_map
      else
	Smap.add g.name [g] gates_map
    end
  )


(* gestion de l'induction *)

(* NOT IMPLEMENTED *)

(* table des blocks *)
let block_tbl = Stbl.create 42 

(* on maintient la taille du graphe dans size *)
let size = ref 0 in
let addVertex g = size :=  size + 1 ; Graph.addVertex g !size
let setLabel g label = Graph.setLabel g !size label

(* convertit un opérateur en étiquette, *TODO* *)
let lop_to_label = ()
let lp_to_label = ()
let bool_to_label = ()


(* construit le nouveau graphe en ajoutant un block et renvoit ce graphe*)
let rec add_block_to_graph gcur b = 
    (* renvoit le couple du nouveau graphe * l'index du noeud de sortie après avoir traité l'expression *)
  let rec process_expr gcur env  = function
    | Bconst cst -> 
      let gcur = addVertex gcur in
      let gcur = setLabel gcur (bool_to_label cst) in
	gcur,!size
    | Bvar ident -> 
      let src = Smap.find ident env in
      gcur,src
    | Bbinop(oper, expr1, expr2) -> 
      let cur = !size in
      let gcur = addVertex gcur in 
      let gcur = setLabel gcur (lop_to_label oper) in
      let gcur,i1 = process_expr gcur expr1 in
      let gcur,i2 = process_expr gcur expr2 in
      let gcur = Graph.addEdge gcur i1 cur in
      let gcur = Graph.addEdge gcur i2 cur in
      gcur,cur
    | Bprefix(oper, expr) ->
      let cur = !size in
      let gcur = addVertex gcur in
      let gcur = setlabel gcur (lp_to_label oper) in
      let gcur,i = process_expr gcur expr in
      let gcur = Graph.addEdge gcur i cur in
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
  let gate = Smap.find b.gate_type gates_map in
  (* on crée l'env *)
  let env = List.fold_left2 add_wire_to_env Smap.empty gate.inputs b.inputs in
  (* on applique process_stmt sur tout les stmts de la porte 
       on obtient le nouveau graphe et un environnement *)
  let gcur,env = List.fold_left process_stmt (gcur,env) gate.body in
  (* Workaround : maintenir la liste des sorties dans process_stmt ?*)
  (* on génère la liste des noeuds de sortie 
       et on ajoute notre block à la table des blocks *)
  let outputs = List.map (fun ident -> Smap.find ident env) gate.outputs in
  Stbl.add block_tbl block.name outputs ;
  gcur
  
(* construit le graphe d'un circuit *)
let buildgraph cir = 
  (* on crée la table des portes *)
  let gates_map = parse_gates Smap.empty cir.gates in
  (* on traite le premier block du circuit *)
  let g,inputs =  List.fold_left (fun (curgraph,li) _ ->
    let li = (!size+1)::li in
    let curgraph = addVertex curgraph in
    let curgraph = setLabel curgraph  Input in
    curgraph,li) Graph.empty cir.start.inputs in
  Stbl.add block_tbl cir.start.name inputs;
  let curgraph = List.fold_left add_block_to_graph curgraph cir.blocks in
  {graph = curgraph, inputs = inputs, outputs = outputs }
    
      
    

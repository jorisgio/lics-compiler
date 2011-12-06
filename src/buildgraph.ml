(* Transformation de la syntaxe abstraite en un graphe *)

open Ast
open Graphe

exception Builderror of string

(* chaines ordonées et hashtbls de chaînes *)
module Str = struct
  include(String)
  let equal x y = ((Pervasives.compare x y) == 0)
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
end
module  Stbl = Hashtbl.Make(Str)

module Int = struct
  type t = int
  let compare = Pervasives.compare
end
  
module Imap = Map.Make(Int)

(* module implémentant l'environnement des variables locales *)
module Smap = Map.Make(String)



(* on ajoute un accumulateur gardant le noeud courant au graphe *)
module Graphe = struct
  include(Graphe)

  (* attention si on supprime des noeuds, il peut y avoir des clés inocupées. anyaway, osef*)
  let next = ref 0
    
  (* ajoute un noeud au graphe d'index le prochain disponible *)
  let addVertex2 g = next :=  !next + 1 ; addVertex g !next
    
  let setLabel2 g label = setLabel g !next label
end



(* on ajoute une fonction pour convertir la syntaxe abstraite en étiquette *)
module Noeud = struct
  include (Noeud)
    
  (* convertit un opérateur en étiquette *)
  let lop_to_label = function
    | Ast.And -> And 
    | Ast.Or -> Or 
    | Ast.Xor -> Xor 

  let lp_to_label   = function
    | Ast.Not -> Not
      
  let bool_to_label = function
    | Ast.Cbool(true) -> True
    | Ast.Cbool(false) -> False
end


(* Fonctions traitant les portes *)
module Gates = struct 
  (*  Un seul noeud pour chaque constante ça parait une bonne idée *)
  let constList = ref []
    
  (* parcours la liste des portes et les ajoutes à une Smap name -> porte *)
  let rec parse_gates gates_map q = match q with
    |[] -> gates_map
    | a::q -> parse_gates (Smap.add a.gname a gates_map) q
  
  let addToLazy lazyList g i1 i2 =  
    if i1 > 0 then
      Graphe.addEdge g i1 i2
    else 
      (lazyList := (i1,i2)::!lazyList ; g)

  (* Traite une expression. 
     Prend un graphe, l'environnement des variables locales à la portes,
     ajoute les noeuds nécessaires au graphe
     renvoit (graphe*noeud courant) 
     ou le noeud courant est le noeud représentant la valeur de "sortie" *)
  let rec process_expr gcur env ll ml = function
    | Bconst cst -> begin
      try
	let cur = List.assoc cst !constList in
	gcur,cur
      with Not_found -> 
       	let gcur = Graphe.addVertex2 gcur in
	let gcur = Graphe.setLabel2 gcur (Noeud.bool_to_label cst) in
	constList := ((cst,!Graphe.next)::!constList);
	(gcur,!Graphe.next)
    end
	
    | Bvar ident -> 
      let src = Smap.find ident env in
      gcur,src
    | Bbinop(oper, expr1, expr2) -> 
      let gcur = Graphe.addVertex2 gcur in 
      let gcur = Graphe.setLabel2 gcur (Noeud.lop_to_label oper) in
      let cur = !Graphe.next in
      let gcur,i1 = process_expr gcur env ll ml expr1 in
      let gcur,i2 = process_expr gcur env ll ml expr2 in
      let gcur = addToLazy ll gcur i1 cur in
      let gcur = addToLazy ll gcur i2 cur in
      gcur,cur
    | Bprefix(oper, expr) ->
      let gcur = Graphe.addVertex2 gcur in
      let gcur = Graphe.setLabel2 gcur (Noeud.lp_to_label oper) in
      let cur = !Graphe.next in
      let gcur,i = process_expr gcur env ll ml expr in
      let gcur = addToLazy ll gcur i cur in
      gcur,cur
    | Bmux(e1,e2,e3) ->
      let  gcur = Graphe.addVertex2 gcur in
      let cur = !Graphe.next in
      let gcur,i1 = process_expr gcur env ll ml e1 in
      let gcur,i2 = process_expr gcur env ll ml e2 in
      let gcur,i3 = process_expr gcur env ll ml e3 in
      let gcur = addToLazy ll gcur i1 cur in
      let gcur = addToLazy ll gcur i2 cur in
      let gcur = addToLazy ll gcur i3 cur in
      let gcur = Graphe.setLabel gcur cur (Noeud.Mux(i1,i2,i3)) in
      if i1 < 0 then ml := (i1,cur,1)::!ml; 
      if i2 <0 then ml := (i2,cur,2)::!ml;
      if i3 <0 then ml := (i3,cur,3)::!ml;
      gcur,cur

  (* Traite une instruction.
     Prend un coupe (graphe courant * env local)
     renvoit ce couple actualisé après ajout de l'instruction*)
  let process_stmt ll ml  (gcur,env) = function
    | Lassign(ident, expr) ->   
      let gcur,out = process_expr gcur env ll ml expr in
      let env = Smap.add ident out env in
      gcur,env
    | _ -> raise Not_implemented
end



module Blocks = struct
(* table des blocks 
   A chaque nom de block, associe un array de clés d'un noeud du graphe
   l'élément i du tableau donne la clé du noeud représentant la ième sortie de 
   ce block *)
  let block_tbl = Stbl.create 42 
    
  (* ajoute un fil à l'environnement des variables formelles de la porte 
     instanciant le block : On relie une entrée formelle de porte à un noeud
     L'environnement associe à chaque identifiant formel d'une variable de porte
     la clé du noeud auquel elle est liée dans ce block *)
  let add_wire_to_env (curenv,curenv2,n) ident wir =
    try
      let varray = Stbl.find block_tbl wir.block_id in 
      ((Smap.add ident varray.(wir.out_id) curenv),curenv2,(n+1))
    with  Not_found -> ((Smap.add ident (-n) curenv),(Imap.add (-n) wir curenv2),(n+1))

  (* ajoute un block au graphe
     prend une table des portes associant une porte à son identifiant
     un graphe intermediaire de type Graphe.interm_graph 
     un block b
     renvoit un élément du même type, contenant le block ajouté *)
  let rec add_block_to_graph gates_map (intermcur,lazyEdges,lazyMux) b = 

    (* on trouve la porte dont le block est une instance *)
    let gate = Smap.find b.bgate_type gates_map in
    (* on crée l'env, en y ajoutant les entrées *)
    let env,wirenv,_ = 
      List.fold_left2 add_wire_to_env (Smap.empty,Imap.empty,1) gate.ginputs b.binputs 
    in
    let ml = ref [] and ll = ref [] in
    let gcur,env = 
      List.fold_left (Gates.process_stmt ll ml) (intermcur.igraph,env) gate.gbody
    in
    let ll = List.map (fun (i1,i2) -> ((Imap.find i1 wirenv),i2)) !ll in
    let ml = List.map (fun (i1,i2,i3) -> ((Imap.find i1 wirenv),i2,i3)) !ml in
    (* on génère la liste des noeuds de sortie 
       et on ajoute notre block à la table des blocks *)
    let outputs = List.map (fun ident -> Smap.find ident env) gate.goutputs in
    Stbl.add block_tbl b.bname (Array.of_list outputs) ;
    ({ igraph = gcur ; ioutputs = outputs ; iinputs = intermcur.iinputs },(ll @ lazyEdges),(ml @ lazyMux))
    
  let process_start_block b =
    let add (curgraph,li) _ =
      let li = (!(Graphe.next) + 1)::li in
      let curgraph = Graphe.addVertex2 curgraph in
      let curgraph = Graphe.setLabel2 curgraph Noeud.Input in
      curgraph,li 
    in
    let g,inputs =  
      List.fold_left add (Graphe.empty,[]) b.binputs
    in
    Stbl.add block_tbl b.bname (Array.of_list inputs);
    g,inputs

  let processLazy graph muxList edgeList =
    let processEdges gr (w1,cur)  =
      let varray = Stbl.find block_tbl w1.block_id in 
      Graphe.addEdge gr varray.(w1.out_id) cur  
    in
    let processMux gr (w1,cur,nb) =
      let varray = Stbl.find block_tbl w1.block_id in
      match (Graphe.getLabel gr cur),nb with
	| Noeud.Mux(_,i2,i3),1 -> Graphe.setLabel gr cur (Noeud.Mux(varray.(w1.out_id),i2,i3))
	| Noeud.Mux(i1,_,i3),2 -> Graphe.setLabel gr cur (Noeud.Mux(i1,varray.(w1.out_id),i3))
	| Noeud.Mux(i1,i2,_),3 -> Graphe.setLabel gr cur (Noeud.Mux(i1,i2,varray.(w1.out_id)))
    in
    let graph = List.fold_left  processEdges graph edgeList in
    List.fold_left processMux graph muxList
    
    
end
  
  
(* construit le graphe d'un circuit *)
let buildgraph cir = 
  (* on crée la table des portes *)
  let gates_map = Gates.parse_gates Smap.empty cir.gates in
  
  (* on traite le premier block du circuit *)
  let g,inputs = Blocks.process_start_block cir.start in
  let g1,lazyEdge,lazyMux = List.fold_left (Blocks.add_block_to_graph gates_map) ({igraph= g; iinputs = inputs; ioutputs = [] },[],[]) cir.blocks in
  { g1 with igraph = Blocks.processLazy g1.igraph lazyMux lazyEdge}

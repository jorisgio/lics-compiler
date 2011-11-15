(* allocation de registres *)
open LicsAst

module I = struct
  type t = int
  let compare = Pervasives.compare
end

module ISet = Set.Make(I)

module Node = struct
  type t = int
  type label = int
  let empty = 0
  let compare = Pervasives.compare
end

(* Graphe d'interference *)

module IGraphe = struct
  include(Builder.MakeLabeledGraph(Node))
	    
  module I2 = struct
    type t = int*int
    let compare = Pervasives.compare
  end

  module I2Set = Set.Make(I2)

  (* On redéfini addEgde pour avoir une symétrie *)	    
  let addEdge g src dst = addEdge (addEdge g src dst) dst src
    
  let removeEdge g src dst = removeEdge (removeEdge g src dst) dst src
    
  let drawGraph g f =
    let printEdge src dst (acc,e) =
      if I2Set.mem (dst,src) e then
	acc,e
      else
	begin
	  let acc = acc ^ (Printf.sprintf "%d -- %d;\n" src dst) in
	  acc,(I2Set.add (src,dst) e)
	end
    in
    let write key value env =
      let dc = Vertex.getLabel value in
      let c = float_of_int dc in
      Printf.fprintf f "%d[label=\"%d→%d\",style=filled,color=\"%f,%f,1\"];\n" key key dc (1./.c) (c/.10.);
      let str,env= Vertex.foldSucc (printEdge key) value ("",env) in
      if String.length str > 0 then Printf.fprintf f "%s" str;
      env
    in
    Printf.fprintf f "graph interferences {\n" ;
    let _ = foldVertex write g I2Set.empty in
    Printf.fprintf f "}"
end

module IVertex = IGraphe.Vertex
    
(* analyse de durée de vie sur le flux séquentiel trié topologiquement *)
module LivenessAnalysis = struct
  type livenessInfo = {
    instr : stmt;
    out_set : ISet.t;
    def_set : int list;
  }
      
  (* calcul des temporaires utilisés par instr *)
  let used instr =
  let used_expr = function
    | Const _ -> ISet.empty
    | Unaire(_,i1) -> ISet.singleton i1
    | Binaire(_,i1,i2) -> ISet.add i1 (ISet.singleton i2)
    | Ternaire(_,i1,i2,i3) -> ISet.add i3 (ISet.add i2 (ISet.singleton i1))
  in
  match instr with
    | Assign(i,e) -> used_expr e
    | Inputreg _ | Input _ -> ISet.empty
    | Outputreg i | Output i -> ISet.singleton i
      
  (* temporaires définis par l'instruction *)
  let def = function
    | Input i | Inputreg i | Assign(i,_) -> [i]
    | _ -> []
    
  (* Supprime les calculs inutiles (aka dont le résultat n'est pas réutilisé *)
  let optimizeNotUsed livelist =
    let deleteInstr is = match is.instr with
      | Assign(i,_) when not (ISet.mem i is.out_set) -> true
      | _ -> false
    in
    ()
      
  let analyse prog =
    (* On pourrait utiliser fold_right, mais rev_map est tail recursive *)
    let cur = ref ISet.empty in
    (* On a In(i) = Use(i) U (Out(i)\Def(i)) *)
    let pointfixe instr = 
      let useds = used instr in
      let diff = match def instr with
	| [] -> !cur
	| [elt] -> ISet.remove elt !cur
	| _ -> failwith "OMG ERROR 42"
      in
      let r = { instr = instr; out_set = !cur ; def_set = def instr } in
      cur := ISet.union diff useds;
      r
    in
    List.rev_map pointfixe (List.rev prog)
      
  (* construit le graphe d'interférence depuis la liste d'analyse de vie *)
  let buildgraph livenesslist =
    (* ajoute une instruction au graphe d'interférence *)
    let addInstr acc elt = 
      match elt.def_set with
	| []-> acc
	| [d] -> let diff = ISet.remove d elt.out_set in
		 ISet.fold (fun tmp g -> IGraphe.addEdge g tmp d ) diff (IGraphe.addVertex acc d)
	| _ -> failwith "OMG ERROR 42"
    in
    List.fold_left addInstr IGraphe.empty livenesslist
      
end


(* Allocation de registres depuis le graphe d'interférence *)
module Alloc = struct
(* le problème se résume à trouver uen k-coloration du graphe, avec k le 
   nombre chromatique du graphe d'interférence. 
   Ce problème est NP-complet
   Il existe une floppée d'heuristiques
   mais on utilise ici un simple algorithme glouton
*)
  exception Found of int
      
  (* renvoit une liste associative des noeuds triés selon leur degrés *)
  let ordering graph =
    let degree vertex = IVertex.foldSucc (fun _ c -> c+1) vertex 0 in
    let degList = IGraphe.foldVertex (fun k v lis -> let d = degree v in ((k,d)::lis)) graph [] in
    List.sort (fun x y -> match (snd y) - (snd x) with
      |a when a > 0 -> 1
      |0 -> 0
      | _ -> -1
    ) degList
  

  let color g =
    let ordered = ordering g in
    let chromNumber = ref 1 in
    let colorVertex graph (k,d)  = 
      let value = IGraphe.find graph k in	
      (* on construit la liste des couleurs voisines *)
      let adjColors = 
	IVertex.foldSucc (fun k set -> let lab =IGraphe.getLabel graph k in ISet.add lab  set) value ISet.empty
      in
      let isAvaible elt cur = 
	if elt = cur 
	then 
	  cur + 1 
	else 
	  raise (Found (elt-1))
      in
      let c =
	try
	  (* d'après le man, l'itération sur un ensemble se fait dans un ordre croissant *)
	  ISet.fold isAvaible (ISet.add 0 adjColors) 0
	with Found elt -> elt 
      in
      if c > !chromNumber then chromNumber:= c ;
      IGraphe.setLabel graph k c
    in
    List.fold_left colorVertex g  ordered
end
  

(* On réunit le tout *)
let process licslist = 
  let livelist = LivenessAnalysis.analyse licslist in
  let uncolored = LivenessAnalysis.buildgraph livelist in
  let colored = Alloc.color uncolored in
  let getReg ident= IGraphe.getLabel colored ident in
  let rec sub_in_expr = function 
    | Const c -> Const c
    | Unaire(o,i) -> Unaire(o,getReg i)
    | Binaire(o,i,i1) -> Binaire(o,getReg i,getReg i1)
    | Ternaire(o,i,i1,i2) -> Ternaire(o,getReg i,getReg i1,getReg i2)
  in
  let sub_in_stmt = function
    | Assign(i,e) -> Assign(getReg i,sub_in_expr e)
    | Input i -> Input(getReg i)
    | Output i -> Output(getReg i)
    | Inputreg i -> Inputreg(getReg i)
    | Outputreg i -> Outputreg(getReg i)
  in
  List.map sub_in_stmt licslist,colored

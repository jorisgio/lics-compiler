(* construit le graphe à partir du langage *)

open Graphe
open Ast
open Sast

(* méthode de construction récursive. 
Pour chaque niveau d'appel, on construit un map qui à chaque identifiant associe un noeud. 
Une fois cela fait, on crée les liens. *) 


(* TYPES ET MODULES *)


exception Undefined
exception Error of pos * string
exception WrongType of pos * types * types
      
module Noeud = struct
  include (Noeud)
    
  (* convertit un opérateur en étiquette *)
  let lop_to_label = function
    | Sast.And -> And 
    | Sast.Or -> Or 
    | Sast.Xor -> Xor 

  let lp_to_label   = function
    | Sast.Not -> Not
    | Sast.Reg -> Reg
      
  let bool_to_label = function
    | Sast.EBconst(true) -> True
    | Sast.EBconst(false) -> False
end

type graphe = { rgraph : Graphe.t ; sizeof : int }



(*TRAITEMENT DES LIENS ENTRE LES NOEUDS *)



(* construit un graphe 
   circuit : le circuit source *)
let pCircuit circuit =
  let graph = Graphe.addVertex Graphe.empty 0 in
  let graph = Graphe.setLabel graph 0 Noeud.True in
  let graph = Graphe.addVertex graph 1  in
  let graph = Graphe.setLabel graph 1 Noeud.False in
  let index = ref 2 in
  
(* Construit un tableau unique de noeuds à partir d'expressions
   ioArray : le tableau
   ind : (une reference ) l'index 
   env : l'env des variables locales
   expr : *)
  let buildArray ioArray ind env  expr = 
    match expr.e with
      | EVar(ident) -> begin
	try
	  let ar = Smap.find ident.id env in
	  for i = 0 to (Array.length ar) - 1 do 
	    ioArray.(!ind) <- ar.(i);
	    incr ind;
	  done;
	with Not_found -> failwith "Variable non liée utilisée en entrée" 
    (* à transformer en erreur plus propre *)
      end
      | EArray_i(id,i) -> begin
	try
	  let ar = Smap.find id.id env in
	  ioArray.(!ind) <- ar.(i) ;
	  incr ind;
	with Not_found -> failwith "Variable non liée utilisée en entrée" 
    (* à transformer en erreur plus propre *)
      end
      | EArray_r(id,i1,i2) -> begin
	try
	  let ar = Smap.find id.id env in
	  for i = i1 to i2 do 
	    ioArray.(!ind) <- ar.(i);
	    incr ind;
	  done;
	with Not_found -> failwith "Variable non liée utilisée en entrée" 
      (* à transformer en erreur plus propre *)
      end
  in
  (* construit un tableau de noeuds à partir d'une expression 
     et d'un tableau de noeud *)
  let addInputs ioArray ind callenv id= 
    match id.typ with 
      | Bool -> 
	let ar = Array.make 1 ioArray.(!ind) in
	incr ind ;
	Smap.add id.id ar callenv 
      | Array n -> 
	let ar = Array.make n (-1) in
	for k = 0 to n - 1 do
	  ar.(k) <- ioArray.(!ind);
	  incr ind 
	done ;
	Smap.add id.id ar callenv
      | _ -> raise (WrongType ({line = 42; char_b = 42; char_e = 42},Int,Bool))
  in

  let addOutputs ioArray ind callenv expr =
    match expr.e with
      | EVar(id) -> 
	let ar = Array.make 1 ioArray.(!ind) in
	incr ind ;
	Smap.add id.id ar callenv
      | EArray_i(id,i) ->
	let Array n = id.typ in
	let ar = 
	  try
	    Smap.find id.id callenv 
	  with Not_found -> Array.make n (-1) 
	in
	ar.(i) <- ioArray.(!ind);
	incr ind;
	Smap.add id.id ar callenv
      | EArray_r(id,i1,i2) ->
	let Array n = id.typ in
	let ar = 
	  try 
	    Smap.find id.id callenv 
	  with Not_found -> Array.make n (-1) 
	in
	for k = i1 to i2 do 
	  ar.(k) <- ioArray.(!ind);
	  incr ind
	done;
	Smap.add id.id ar callenv
  in 
	
  (* Traite une expression de type Bool récursivement en ajoutant les arrêtes aux graphe 
     Prend :
     gates : la map des portes
     gcur : graphe courant
     env : map qui à chaque variable associe un tableau des noeuds
     vertex : noeud auquel on doit lier l'expression 
     expr : l'expression
     Renvoit :
     le nouveau graphe *)
  let rec  processBExpr gcur env vertex expr  =
    let rec  processRec gcur vertex expr = 
      match expr.e with
	| EBconst cst -> 
	  assert (Smap.mem (string_of_bool cst) env) ;
	  let cur = Smap.find (string_of_bool cst) env in
	  let gcur = Graphe.setLabel gcur cur.(0) (Noeud.bool_to_label (Sast.EBconst(cst))) in
	  Graphe.addEdge gcur cur.(0) vertex
	    
	| EVar ident ->
	  Printf.printf "%s" ident.id ; 
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
          let i = !index in
          (* on est obligé de créer d'abord les noeuds parents pour pouvoir en
             conserver leur indice dans l'étiquette de Mux *)
	  let gcur = processRec gcur !index exp1 in
          let i2 = !index in
	  let gcur = processRec gcur !index exp2 in
          let i3 = !index in
	  let gcur = processRec gcur !index exp3 in
	  let gcur = Graphe.setLabel gcur i (Noeud.Mux (i + 1, i2 + 1, i3 + 1) )
          in (* les noeuds des parents de Mux *)
	  Graphe.addEdge gcur i vertex
    in
    let g = 
      match expr.e with
	| EBconst cst -> Graphe.setLabel gcur vertex.(0) (Noeud.bool_to_label (Sast.EBconst(cst))) 
	| EPrefix(oper, exp) -> 
	  let gcur = Graphe.setLabel gcur vertex.(0) (Noeud.lp_to_label oper) in
	  processRec gcur vertex.(0) exp 
	| EInfix(oper, exp1,exp2) -> 
	  let gcur = Graphe.setLabel gcur vertex.(0) (Noeud.lop_to_label oper) in
	  let gcur = processRec gcur vertex.(0) exp1 in
	  processRec gcur vertex.(0) exp2
	| EMux(exp1,exp2,exp3) ->
          (* on est obligé de créer d'abord les noeuds parents pour pouvoir en
             conserver leur indice dans l'étiquette de Mux *)
          let i1 = !index in
	  let gcur = processRec gcur vertex.(0) exp1 in
          let i2 = !index in
	  let gcur = processRec gcur vertex.(0) exp2 in
          let i3 = !index in
	  let gcur = processRec gcur vertex.(0) exp3 in
	  let gcur = Graphe.setLabel gcur in
            vertex.(0) (Noeud.Mux (i1 + 1, i2 + 1, i3 + 1) )
        (* là c'est plus compliqué, on descend dans une autre frame *)
	| ECall(gatename,args) ->
	  begin
	    let callgate = 
	      try 
		Smap.find gatename circuit.gates 
	      with Not_found -> (raise Undefined)
	    in
	  (* on transforme l'entrée de la porte en un unique tableau de noeud. *)
	    let inputsArray = Array.make callgate.ginputsize (-1) in 
	    let aindex = ref 0 in 
	    
	    let () =
	      try 
		List.iter (buildArray inputsArray aindex env)  args
	      with Invalid_argument _ -> (
		raise (Error ({line = 42; char_b = 42; char_e = 42},
			      "Pas le bon nombre d'entrée pour l'appel " )))
	    in
	    let () = if !aindex < callgate.ginputsize then
		(raise (Error ({line = 42; char_b = 42; char_e = 42},
			       "Pas le bon nombre d'entrée pour l'appel " )))
	    in
	    aindex := 0 ; 
	    
		
	  (* on ajoute tout les arguments formels à l'env fils
	     c'est à dire que à chaque entrée formelle de la porte
	     on associe un tableau de noeuds *) 
	    let callenv = List.fold_left (addInputs inputsArray aindex ) Smap.empty callgate.ginputs in
	    (* On ajoute aussi les noeuds de sortie, qui ont déjà été créés.
	       cela permet d'utiliser des noeuds définis par un appel postérieur*)
	    if (Array.length vertex) != callgate.goutputsize then failwith "Mauvais nombre de sortie";
	    aindex := 0;
	    let callenv = List.fold_left (addOutputs vertex aindex) callenv callgate.goutputs in
	    (* il suffit d'appeler pLevel recursivement avec ce nouvel env,
	       on obtient un nouveau graphe *)
	    (snd (pLevel callenv gcur gatename))
	  end
	    
	| _ -> failwith "You cannot assign a left value to a left value"
    in
    g
      
  (* AJoute l'instruction au graphe.
     Prend :
     gcur : le graphe courant
     env : une map qui à chaque ident associe un tableau des noeuds du graphe
     instr : l'instruction
     Renvoit :
     le nouveau graphe 
  *)
  (* TODO : for pour assigner  une valeur à un index d'un tableau  *)
  and processInstr env gcur instr = 
    match instr.i with 
    | Assign(ident, exp) ->
      assert (Smap.mem ident.id env) ;
      let cur = Smap.find ident.id env in
      processBExpr gcur  env cur  exp
    | Assign_i(ident, i, exp) ->
      let cur = Smap.find ident.id env in
      processBExpr gcur  env [|cur.(i)|] exp 
    | Assign_r(ident, i1, i2, exp) ->
      let cur = Smap.find ident.id env in
      processBExpr gcur  env (Array.sub cur i1 (i2 - i1 + 1) ) exp
    (* on a l'env de tout les idents définis DANS le for 
       ils ne sont visibles qu'a l'intérieur du bloc *)
    (* Manière de procéder : on traite simplement le tout n fois 
       en substituant la variable entière comme il faut *)
    | For(fenv,id,i1,i2,li) ->
      (* traitement pour une itération 
      TODO : gérer la substitution de la variable d'itération *)
      let i2 = i2 + 1 in
      let rec atomic graph =  function
	| i2  -> graph
	| i -> 
	(* d'abord, on ajoute les noeuds *)
	  let createNodes name ident (fenv,graph) =
	    match ident.typ with
	      | Bool -> begin
		let graph = Graphe.addVertex graph !index in
		let ar = Array.make 1 !index in
		incr index ;
		((Smap.add name ar env),graph)
	      end
	      | Array s -> begin
		let ar = Array.make s 0 in
		let gr = ref graph in
		for i = 0 to (s - 1) do
	          gr := Graphe.addVertex !gr !index ;
		  ar.(i) <- !index ;
                  incr index ;
		done ;
		((Smap.add name ar env), !gr)
	      end
	      | Int -> failwith "Int : not implemented"
	  in
	  let env,graph = Smap.fold createNodes fenv (env,graph) in
	  (* ensuite, on crée les liens, comme d'hab :Þ *)
	  let graph  = List.fold_left (processInstr env) graph li in
	  atomic graph (i+1)
      in
      atomic graph i1 
    |Decl(_) -> gcur
    | _ -> failwith "Oh ! Not implemented"
      
      
  (* construit récursive|ment le graphe, niveau par niveau
     env : l'environnement hérité du niveau supérieur (aka les arguments)
     graph : le graphe courant
     index : la taille
     gatename : le nom  porte du niveau courant 
     renvoit un tableau des noeuds de sortie *)
  and pLevel env graph gatename  = 
    let gate = Smap.find gatename circuit.gates in
    (* lit le genv d'une porte 
       ajoute un noeud pour chaque identifiant du graphe
       sauf pour les entrées (qui ne sont pas des noeuds, mais de liens)
       name : nom de l'id
       ident : l'id 
       graph : le graphe courant
       env : map qui associe un noeud à un identifiant *)
    let  createNodes name ident (env,graph) =
      if (List.mem ident gate.ginputs)
      then 
	(env,graph) 
      else
	(* si on a déjà des noeuds pour cet ident, 
	   on vérifie juste qu'il y a bien un noeud pour chaque élément *)
	if (Smap.mem ident.id env) then
	  
	  let g = Array.fold_left (fun graph noeud -> 
	      if noeud = (-1) then ( incr index ; Graphe.addVertex graph (!index -1 )) else graph ) graph (Smap.find ident.id env) 
	  in
	  (env,g)
	      
	else
	begin
	  match ident.typ with
	    | Bool -> begin
	      let graph = Graphe.addVertex graph !index in
	      let ar = Array.make 1 !index in
              incr index ;
	      ((Smap.add name ar env),graph)
	    end
	    | Array s -> begin
	      let ar = Array.make s 0 in
	      let gr = ref graph in
	      for i = 0 to (s - 1) do
	        gr := Graphe.addVertex !gr !index ;
		ar.(i) <- !index ;
                incr index ;
	      done ;
	      ((Smap.add name ar env), !gr)
	    end
	    | Int -> failwith "Int : not implemented"
	end
    in
    (* on ajoute vrai/faux à l'environnement *)
    let env = Smap.add "true" [|0|] env in
    let env = Smap.add "false" [|1|] env in
    (* on ajoute les noeuds *)
    let env,graph = Smap.fold createNodes gate.genv (env,graph) in
    
    
    (* Well, maintenant il faut relier tout ces noeuds créés *)
    (env,(List.fold_left (processInstr env) graph gate.gbody))
  in
  
  (* Ok on a juste à appeler ça sur la porte principale du circuit maintenant
   et à traiter les entrées*)
    
  (* par convention, la porte principale s'appelle "Start" *)
  let start = Smap.find "Start" circuit.gates in
  (* On ajoute les entrées *)
  let ar = Array.make (start.ginputsize) 0 in
  for j = 0 to start.ginputsize - 1 do 
    ar.(j) <- !index ;
    incr index
  done;
  let i = ref 0 in
  let env = List.fold_left (addInputs ar i) Smap.empty start.ginputs in
  let iList = Array.to_list ar in
  let graph = List.fold_left (fun graph elt -> 
    let graph = Graphe.addVertex graph elt in
    Graphe.setLabel graph elt Noeud.Input ) graph iList in

  let env,graph = pLevel env graph "Start" in
  
  (* on s'occupe maintenant des sorties *)
  i := 0;
  let ar = Array.make (start.goutputsize) 0 in
  List.iter (buildArray ar i env) start.goutputs;
  let oList = Array.to_list ar in
  { igraph = graph ; iinputs = iList; ioutputs = oList }
  
    

  
    
  
  

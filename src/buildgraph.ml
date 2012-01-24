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


let op_to_op = function
  | Add -> (+)
  | Sub -> (-)
  | Mul -> ( * )
  | Div -> (/)
    
(*  eval an integer expression *)
let rec eval env expr = match expr.e with 
  | EIconst i -> i
  | EVar id -> assert(Smap.mem id.id env) ;Smap.find id.id env 
  | EPrefix(Minus,e) -> let i = eval env e in
			-i
  | EInfix(op,e1,e2) -> let i1 = eval env e1 in
			let i2 = eval env e2 in
			let op = op_to_op op in
			op i1 i2
			  

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
   intEnv : l'environnement des valeurs entières
   expr : *)
  let buildArray ioArray ind intEnv  env  expr = 
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
	let i = eval intEnv i in
	try
	  let ar = Smap.find id.id env in
	  ioArray.(!ind) <- ar.(i) ;
	  incr ind;
	with Not_found -> failwith "Variable non liée utilisée en entrée" 
    (* à transformer en erreur plus propre *)
      end
      | EArray_r(id,i1,i2) -> begin
	let i1 = eval intEnv i1 in
	let i2 = eval intEnv i2 in
	try
	  let ar = Smap.find id.id env in
	  for i = i1 to i2 do
            assert (!ind < Array.length ioArray);
            assert (i < Array.length ar);
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

  let addOutputs ioArray ind intEnv callenv expr =
    match expr.e with
      | EVar(id) -> 
	let ar = Array.make 1 ioArray.(!ind) in
	incr ind ;
	Smap.add id.id ar callenv
      | EArray_i(id,i) ->
	let i = eval intEnv i in
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
	let i1 = eval intEnv i1 in
	let i2 = eval intEnv i2 in
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
  let rec  processBExpr gcur intEnv env vertex expr  =
    let rec  processRec gcur vertex expr =
      match expr.e with
	| EBconst cst -> 
	  assert (Smap.mem (string_of_bool cst) env) ;
	  let cur = Smap.find (string_of_bool cst) env in
	  let gcur = Graphe.setLabel gcur cur.(0) (Noeud.bool_to_label (Sast.EBconst(cst))) in
	  Graphe.addEdge gcur cur.(0) vertex , cur.(0)
	| EVar ident ->
	  assert (Smap.mem ident.id env) ;
	  let cur = Smap.find ident.id env in
	  Graphe.addEdge gcur cur.(0) vertex , cur.(0)
	| EArray_i(ident,index) ->
	  let index = eval intEnv index in
	  assert (Smap.mem ident.id env) ;
	  let cur = Smap.find ident.id env in
	  Graphe.addEdge gcur cur.(index) vertex, cur.(index)
	| EPrefix(oper,exp) -> 
	  let gcur = incr index ; Graphe.addVertex gcur !index in
          let i = !index in 
	  let gcur = Graphe.setLabel gcur !index  (Noeud.lp_to_label oper) 
          in
	  let gcur = Graphe.addEdge gcur !index vertex in
	  let gcur , _ = processRec gcur !index  exp in
          gcur , i
	| EInfix(oper, exp1, exp2) -> 
	  let gcur = incr index; Graphe.addVertex gcur !index in
	  let i = !index in
          let gcur = Graphe.setLabel gcur i (Noeud.lop_to_label oper)
          in
	  let gcur = Graphe.addEdge gcur !index vertex in
	  let gcur , _ = processRec gcur !index exp1 in
	  let gcur , _ = processRec gcur !index exp2 in
          gcur , i
	| EMux(exp1,exp2,exp3) ->
	  let gcur = incr index; Graphe.addVertex gcur !index in
          let i = !index in
          (* on est obligé de créer d'abord les noeuds parents pour pouvoir en
             conserver leur indice dans l'étiquette de Mux *)
	  let gcur , i1 = processRec gcur !index exp1 in
	  let gcur , i2 = processRec gcur !index exp2 in
	  let gcur , i3 = processRec gcur !index exp3 in
	  let gcur = Graphe.setLabel gcur i (Noeud.Mux (i1, i2, i3) )
          in (* les noeuds des parents de Mux *)
	  Graphe.addEdge gcur i vertex , i
    in
    let g = 
      match expr.e with
	| EBconst cst -> Graphe.setLabel gcur vertex.(0) (Noeud.bool_to_label (Sast.EBconst(cst))) 
	| EPrefix(oper, exp) -> 
	  let gcur = Graphe.setLabel gcur vertex.(0) (Noeud.lp_to_label oper) in
	  let gcur , _ = processRec gcur vertex.(0) exp in
          gcur
	| EInfix(oper, exp1,exp2) -> 
	  let gcur = Graphe.setLabel gcur vertex.(0) (Noeud.lop_to_label oper) in
	  let gcur , _ = processRec gcur vertex.(0) exp1 in
	  let gcur , _ = processRec gcur vertex.(0) exp2 in
          gcur
	| EMux(exp1,exp2,exp3) ->
          (* on est obligé de créer d'abord les noeuds parents pour pouvoir en
             conserver leur indice dans l'étiquette de Mux *)
	  let gcur , i1 = processRec gcur vertex.(0) exp1 in
	  let gcur , i2 = processRec gcur vertex.(0) exp2 in
	  let gcur , i3 = processRec gcur vertex.(0) exp3 in
	  Graphe.setLabel gcur vertex.(0) (Noeud.Mux (i1, i2, i3) )
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
		List.iter (buildArray inputsArray aindex intEnv env)  args
	      with Invalid_argument _ -> (
		raise (Error ({line = 42; char_b = 42; char_e = 42},
			      "Pas le bon nombre d'entrée pour l'appel " )))
	    in (*
	    let () = if !aindex < callgate.ginputsize then
		(raise (Error ({line = 42; char_b = 42; char_e = 42},
			       "Pas le bon nombre d'entrée pour l'appel " )))
	    in *)
	    aindex := 0 ; 
	    
		
	  (* on ajoute tout les arguments formels à l'env fils
	     c'est à dire que à chaque entrée formelle de la porte
	     on associe un tableau de noeuds *) 
	    let callenv = List.fold_left (addInputs inputsArray aindex ) Smap.empty callgate.ginputs in
	    (* On ajoute aussi les noeuds de sortie, qui ont déjà été créés.
	       cela permet d'utiliser des noeuds définis par un appel postérieur*)
	    if (Array.length vertex) != callgate.goutputsize then failwith "Mauvais nombre de sortie";
	    aindex := 0;
	    let callenv = List.fold_left (addOutputs vertex aindex intEnv) callenv callgate.goutputs in
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
  and processInstr intEnv env gcur instr = 
    match instr.i with 
    | Assign(ident, exp) ->
      assert (Smap.mem ident.id env) ;
      let cur = Smap.find ident.id env in
      processBExpr gcur  intEnv env cur  exp
    | Assign_i(ident, i, exp) ->
      assert(Smap.mem ident.id env);
      let i = eval intEnv i in
      (*print_int i ;*)
      let cur = Smap.find ident.id env in
      processBExpr gcur intEnv env [|cur.(i)|] exp 
    | Assign_r(ident, i1, i2, exp) ->
      let i1 = eval intEnv i1 in
      let i2 = eval intEnv i2 in
      assert(Smap.mem ident.id env);
      let cur = Smap.find ident.id env in
      processBExpr gcur intEnv env (Array.sub cur i1 (i2 - i1 + 1) ) exp
    | Lw (name, i1, i2, name_a, i1_a, i2_a) ->
      assert (Smap.mem name env);
      assert (Smap.mem name_a env);
      let adresse = Smap.find name_a env in
      let adresse_l = Array.to_list adresse in
      let sortie = Smap.find name env in (* là où il faut stocker *)
      let sortie_l = Array.to_list sortie in
      let gcur = Graphe.addVertex gcur !index in
      let gcur = Graphe.setLabel
        gcur !index (Noeud.Lw (sortie_l , adresse_l)) in
      (* on rajoute une arête avec chacune de ses sorties (qui resteront des
         noeuds vides et de ses entrées *)
      let gcur = List.fold_left
        (fun gcur i -> Graphe.addEdge gcur !index i)
        gcur
        sortie_l in
      List.fold_left
        (fun gcur i -> Graphe.addEdge gcur i !index)
        gcur
        adresse_l
    (* on a l'env de tout les idents définis DANS le for 
       ils ne sont visibles qu'a l'intérieur du bloc *)
    (* Manière de procéder : on traite simplement le tout n fois 
       en substituant la variable entière comme il faut *)
    | For(fenv,ienv,id,i1,i2,li) ->
      (* traitement pour une itération *)
      (* print_endline
         ("For i = " ^ string_of_int i1 ^ " to " ^ string_of_int i2); *)
      let i2 = i2 + 1 in
      let rec atomic graph i =
        if i = i2 then graph
        else begin
	  (* print_endline "Un passage";*)
	  (* d'abord, on ajoute les noeuds *)
	  let createNodes name ident (fenv,graph) =
	    match ident.typ with
	      | Bool -> begin
	        let graph = Graphe.addVertex graph !index in
	        let ar = Array.make 1 !index in
	        incr index ;
	        ((Smap.add name ar fenv),graph)
	      end
	      | Array s -> begin
	        let ar = Array.make s 0 in
	        let gr = ref graph in
	        for i = 0 to (s - 1) do
	          gr := Graphe.addVertex !gr !index ;
		  ar.(i) <- !index ;
                  incr index ;
	        done ;
	        ((Smap.add name ar fenv), !gr)
	      end
	  in
	
	  let env,graph = Smap.fold createNodes fenv (env,graph) in
          (* print_endline "On crée les liens"; *)
	  (* ensuite, on crée les liens, comme d'hab :Þ *)
          let ienv = Smap.add id i ienv in
	  let graph  = List.fold_left (processInstr ienv env) graph li in
	  
	  atomic graph (i+1)
        end
	  
      in
      atomic gcur i1

    |Decl(_) -> gcur
    | _ -> failwith "Oh ! Not implemented"
      
      
  (* construit récursive|ment le graphe, niveau par niveau
     env : l'environnement hérité du niveau supérieur (aka les arguments)
     graph : le graphe courant
     index : la taille
     gatename : le nom  porte du niveau courant 
     renvoit un tableau des noeuds de sortie *)
  and pLevel env graph gatename  = 
    assert( Smap.mem gatename circuit.gates );
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
	begin
	(* si on a déjà des noeuds pour cet ident, 
	   on vérifie juste qu'il y a bien un noeud pour chaque élément *)
	  if (Smap.mem ident.id env) then
	    let ar = (Smap.find ident.id env) in
	    let iz = ref 0 in
	    let g = Array.fold_left (fun graph noeud -> 
	      if noeud = (-1) then ( incr index ;ar.(!iz) <- (!index -1) ; incr iz; Graphe.addVertex graph (!index -1 )) else graph ) graph ar 
	    in
	    ((Smap.add ident.id ar env),g)
	      
	  else
	    begin
	      match ident.typ with
		| Bool -> begin
		  let graph = Graphe.addVertex graph !index in
		  let ar = Array.make 1 !index in
		  incr index ;
		  (*Printf.printf "created %d\n" (!index -1);*)
		  ((Smap.add name ar env),graph)
		end
		| Array s -> begin
		  let ar = Array.make s (-1) in
		  let gr = ref graph in
		  for i = 0 to (s - 1) do
	            gr := Graphe.addVertex !gr !index ;
		    (*Printf.printf "created %d\n" (!index);*)
		    ar.(i) <- !index ;
                    incr index ;
		  done ;
		  ((Smap.add name ar env), !gr)
		end
		| Int -> env,graph
	    end
	end
    in
    (* on ajoute vrai/faux à l'environnement *)
    let env = Smap.add "true" [|0|] env in
    let env = Smap.add "false" [|1|] env in
    (* on ajoute les noeuds *)
    let env,graph = Smap.fold createNodes gate.genv (env,graph) in
    assert((List.length gate.gbody) <> 0);
    (* Well, maintenant il faut relier tout ces noeuds créés *)
    (env,(List.fold_left (processInstr gate.gintEnv env) graph gate.gbody))
  in
  
  (* Ok on a juste à appeler ça sur la porte principale du circuit maintenant
   et à traiter les entrées*)
    
  (* par convention, la porte principale s'appelle "Start" *)
  assert(not (Smap.is_empty circuit.gates));
  (*Smap.iter (fun k _ -> Printf.printf "<%s>\n" k) circuit.gates; *)
  assert(Smap.mem "Start" circuit.gates);
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
  (* print_int start.goutputsize ;*)
  let ar = Array.make (start.goutputsize) 0 in
  List.iter (buildArray ar i start.gintEnv env) start.goutputs;
  let oList = Array.to_list ar in

  { igraph = graph ; iinputs = iList; ioutputs = oList }
  
    

  
    
  
  

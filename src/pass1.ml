(* parcours des blocs pour construire un graphe ne contenant que des noeuds.
   On ajoute un noeud pour chaque variable locale.
   Pour chaque bloc, on construit un environnement qui 
   à chaque identifiant de vairiable fait correspondre un numéro de noeud 

   On s'occupe aussi des sorties*)
open Graphe
open Ast
open Bast

(* Cosntruit un arbre de syntaxe Bast depuis un arbre de syntaxe Sast *)
let pBloc circuit =
  (* Transforme un bloc. 
     Renvoit un graphe * env * wirEnv *)

  let graph = Graphe.addVertex Graphe.empty 0 in
  let graph = Graphe.setLabel graph 0 Noeud.True in
  let graph = Graphe.addVertex graph 1  in
  let graph = Graphe.setLabel graph 1 Noeud.False in
  let index = ref 2 in (* prochain indice disponible dans le graphe *)
  let blocRec bloc (accList,graph,wirEnv) = 
    assert (Smap.mem bloc.bgate_type circuit.gates) ;
    let gate = Smap.find bloc.bgate_type circuit.gates in 
    (* crée un noeud pour chaque variable, excepté les entrées. 
       Construit une Map qui à chaque identifiant associe un tableau de noeuds *)
    let createNodes name ident (env,graph) =
      if (List.mem ident gate.ginputs) then (env,graph) else
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
	    | Int ->failwith "OMG ERROR 42 §"
	end
    in
    (* on définit l'environnement *)
    let env = Smap.add "true" [|0|] Smap.empty in
    let env = Smap.add "false" [|1|] env in
    let env,graph = Smap.fold createNodes gate.genv (env,graph)  in
    
    
    (* on transforme la liste d'expression en sortie d'un bloc en un tableau de noeuds 
       Prend :
       un element de la liste
       renvoit :
       unit
    *)
    let outputsArray = Array.make gate.goutputsize (-1) in 
    
    let ind = ref 0 in
    let buildArray expr = 
      match expr.e with
	| EVar(ident) -> begin
          try
	    let ar = Smap.find ident.id env in
	    for i = 0 to (Array.length ar) - 1 do 
              outputsArray.(!ind) <- ar.(i);
	      incr ind;
	    done;
          with Not_found -> failwith "Utilisation de variables d'entrées en sortie : interdit" (* à transformer en erreur plus propre *)
        end
	| EArray_i(id,i) -> begin
          try
	    let ar = Smap.find id.id env in
	    outputsArray.(!ind) <- ar.(i) ;
	    incr ind;
          with Not_found -> failwith "Utilisation de variables d'entrées en sortie : interdit" (* à transformer en erreur plus propre *)
        end
	| EArray_r(id,i1,i2) -> begin
          try
	    let ar = Smap.find id.id env in
	    for i = i1 to i2 do 
	      outputsArray.(!ind) <- ar.(i);
	      incr ind;
	    done;
          with Not_found -> failwith "Utilisation de variables d'entrées en sortie : interdit" (* à transformer en erreur plus propre *)
        end
    in
    let () = List.iter buildArray gate.goutputs in 
    
    ((({b_bname = bloc.bname; b_bgate_type = bloc.bgate_type; b_binputs = bloc.binputs ; b_bvertices = env })::accList),graph,(Smap.add bloc.bname outputsArray wirEnv)) 
  in
  (* On traite tout les blocs du circuit *)
  (* Dans cet ordre, les blocs restent dans le bon ordre *)
  let blockList,graph,wirEnv = List.fold_right blocRec circuit.blocks ([],(graph),(Smap.empty)) in
  { b_gates = circuit.gates;
    b_blocks = blockList;
    b_inputs = circuit.inputs ;
    b_outputs = circuit.outputs ;
    b_graphe = graph;
    b_blocsOutput = wirEnv }
	   


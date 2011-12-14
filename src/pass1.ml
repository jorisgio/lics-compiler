(* parcour des blocs pour construire un graphe ne contenant que des noeuds.
   On ajoute un noeud pour chaque variable locale.
   Pour chaque bloc, on construit un environnement qui 
   à chaque identifiant de vairiable fait correspondre un numéro de noeud 

   On s'occupe aussi des sorties*)
open Graphe
(* Pour chaque bloc *)
let pBloc blocList =
  (* Transfore un bloc. 
     Rnvoit un graphe * env *)
  let blocRec bloc (graph,env) = 
    let gate = Smap.find bloc.bgate_type gatEnv in 
    (* crée un noeud pour chaque variable, excepté les entrées. 
       Construit une Map qui à chaque identifiant associe un tableau de noeuds *)
    let createNodes name ident (env,graph) =
      if (List.mem ident gate.ginputs) then (env,graph) else
	begin
	  match ident.typ with
	    | Bool -> 
	      let graph = Graphe.addVertex2 graph in
	      let ar = Array.make 1 0 in
	      ar.(0) <- Graphe.!index ;
	      ((Smap.add name  ar env),graph)
	    | Array s -> 
	      let ar = Array.make s 0 in
	      for i = 0 to (s - 1) do
		let graph = Graphe.addVertex2 graph in
		ar.(i) <- Graphe.!index ;
	      done ;
	      ((Smap.add name ar env), graph)
	    | Int ->failwith "OMG ERROR 42 §"
	end
    in
    (* on définit l'environnement *)
    let index = ref 0 in
    let graph = Graphe.addVertex Graphe.empty !index in
    let graph = Graphe.setLabel graph !index Noeud.True in
    incr index;
    let graph = Graphe.addVertex graph !index  in
    let graph = Graphe.setLabel2 graph !index Noeud.False in
    incr index;
    let env = Smap.add "True" [|0|] Smap.empty in
    let env = Smap.add "False" [|1|] env in
    let env,graph = Smap.fold createNodes gate.genv in
    
    
    (* on transforme la liste d'expression en sortie d'un bloc en un tableau de noeuds 
       Prend :
       un element de la liste
       renvoit :
       unit
    *)
    let outputsArray = Array.make gate.goutputsize 0 in 
    let buildArray index expr.e = 
      let ind = ref index in
      match expr.e with
	| Evar(ident) -> 
	  let ar = Smap.find ident.id env in
	  for i = 0 to (Array.length ar) - 1 do 
            ouptutsArray.(!ind) <- ar.(i);
	    incr ind;
	  done;
	  !ind
	| EArray_i(id,i) -> 
	  let ar = Smap.find ident.id env in
	  outputsArray.(!ind) <- ar.(i) ;
	  incr ind;
	  !ind
	| EArray_r(id,i1,i2) ->
	  let ar = Smap.find ident.id env in
	  for i = i1 to i2 do 
	    outputsArray.(!ind) <- ar.(i);
	    incr ind;
	  done;
	  !ind 
	    
      
	    

	    


    
    
	  
	

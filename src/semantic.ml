(* Analyse sémantique de la syntaxe abstraite 
   On réalise le typage des expressions, et on vérifie que tout les id sont bien définis
   On type ensuite les instructions
   En sortie, on obtient une syntaxe abstraite telle que définie par Sast.*)


open Ast

module Exceptions = struct
  open Sast
  exception Undefined of pos * string
  exception Error of pos * string
  exception WrongType of pos * types * types 
      
end

(* affiche un warning *)
let pWarning = () 
  
  

(* On type les expressions et les instructions *)
module InstrToSast = struct
  open Past
  open Exceptions
    
  (* Renvoit un type de type Sast *)
  let typToSast = function
    | Bool -> Sast.Bool
    | Int -> Sast.Int
    | Array s-> Sast.Array s
      
  (* renvoit un id de type Sast *)
  let idToSast id = 
    { Sast.id = id.id ; Sast.typ = typToSast id.typ}
      
  (* renvoit une pos de type Sast *)
  let posToSast pos = 
    { Sast.line = pos.line; Sast.char_b = pos.char_b ; Sast.char_e = pos.char_e }

  let prefixToSast = function
    | Not -> Sast.Not
    | Reg -> Sast.Reg
    | Minus -> Sast.Minus
       
  (* Type les expressions. 
     Prend :
     env : l'environnement des variables locales déjà définies
     undefMap : l'ensemble des idents définis ou utilisés.
     à chaque ident, on associe un tableau. L'index est à 0 si non défini et non utilisé, 1 si défini et utilisé, 2 si non défini et utilisé
     exp : l'expression
     Renvoit : une expression typée * la map des variables non définies * l'ensemble 
  *)
  let rec pExpr env undefSet exp = 
    match exp.e with
      | EBconst b ->
	({ Sast.p = posToSast exp.p ; Sast.e = Sast.EBconst(b) ; Sast.t = Sast.Bool },undefSet)
	  
      | EIconst i -> 
	({ Sast.p = posToSast exp.p ; Sast.e = Sast.EIconst(i); Sast.t = Sast.Int},undefSet)

      | EArray_i(name,index) -> 
	(* on cherche le tableau dans l'env, forcément déclaré *)
        if not (Smap.mem name env) then (raise (Undefined(exp.p,name)));
	let id = Smap.find name env in	 
	({ Sast.p = posToSast exp.p ; Sast.e = Sast.EArray_i(id,index); Sast.t = Sast.Bool},undefSet)
	  
      | EArray_r(name,i_beg,i_end) ->
        assert (Smap.mem name env);
	let id = Smap.find name env in
	({ Sast.p = posToSast exp.p; Sast.e = Sast.EArray_r(id,i_beg,i_end); Sast.t = Sast.Array(i_end - i_beg)},undefSet)
	  
      | EVar(id) ->     

	let {Sast.typ = typ},set = 
	  try 
	  ((Smap.find (id.id) env),undefSet)
	  with Not_found -> ({Sast.id = id.id; typ = Sast.Bool},(Sset.add id.id undefSet))
	in
	(({Sast.p = posToSast exp.p; Sast.e = Sast.EVar(idToSast id) ; Sast.t = typ}),set)
	  
      | EPrefix(p,ex) -> 
	let e,undefSet = pExpr env undefSet ex in
	let ty,sp =
	  match p with
	    | Minus -> if e.Sast.t != Sast.Int then 
		(raise (WrongType(e.Sast.p,e.Sast.t,Sast.Int)))
	      else 
		Sast.Int,Sast.Minus
	    |  Not  -> if e.Sast.t != Sast.Bool then 
		(raise (WrongType(e.Sast.p,e.Sast.t,Sast.Bool)))
	      else
		Sast.Bool,Sast.Not
	    | Reg -> if e.Sast.t != Sast.Bool then
		(raise (WrongType(e.Sast.p,e.Sast.t,Sast.Bool)))
	      else
		Sast.Bool,Sast.Reg
	in
	({ Sast.p = posToSast exp.p ; e = Sast.EPrefix(prefixToSast p,e); t = ty},undefSet)
      | EInfix(i,ex1,ex2) -> 
	let e1,undefMap = pExpr env undefSet ex1 in
	let e2,undefMap = pExpr env undefSet ex2 in
	let ty,si =
	  match i with 
	    | Add -> Sast.Int,Sast.Add
	    | Sub -> Sast.Int,Sast.Sub
	    | Mul -> Sast.Int,Sast.Mul
	    | Div -> Sast.Int,Sast.Div
	    | And -> Sast.Bool,Sast.And
	    | Or -> Sast.Bool,Sast.Or
	    | Xor -> Sast.Bool,Sast.Xor
	in
	if e1.Sast.t != ty then
	  (raise (WrongType(e1.Sast.p,e1.Sast.t,ty)))
        else if e2.Sast.t != ty then
	  (raise (WrongType(e2.Sast.p,e2.Sast.t,ty)))
	else
	  ({Sast.p = posToSast exp.p ; Sast.e = Sast.EInfix(si,e1,e2); Sast.t = ty},(undefSet))
      | EMux(ex1,ex2,ex3) ->
	let e1,undefSet = pExpr env undefSet ex1 in
	let e2,undefSet = pExpr env undefSet ex2 in
        let e3,undefSet = pExpr env undefSet ex3 in

	if e1.Sast.t != Sast.Bool then
	  (raise (WrongType(e1.Sast.p,e1.Sast.t,Sast.Bool)))
        else if e2.Sast.t != Sast.Bool then
	  (raise (WrongType(e2.Sast.p,e2.Sast.t,Sast.Bool)))
	else if e3.Sast.t != Sast.Bool then
	  (raise (WrongType(e3.Sast.p,e3.Sast.t,Sast.Bool)))
        else
          ({Sast.p = posToSast exp.p ; Sast.e = Sast.EMux(e1,e2,e3); Sast.t = Sast.Bool},(undefSet))
      | ECall(name,args) ->
	let el,undefSet = List.fold_right (fun exp (el,undefSet) -> 
	  let e,undefSet = pExpr env undefSet exp in
	  ((e::el),undefSet ) args ([],undefSet)  in
	  ({Sast.p = posToSast exp.p; Sast.e = Sast.ECall(name,el); Sast.t = Sast.Array(List.length el) },(undefMap))
    



  (* Typage des Instructions 
     Prend :
     env : l'environnement des variables locales déjà définies 
     undefMap : Map des ident utilisés et ou définis 
     inst : l'instruction à traiter
     Renvoit : 
     une instruction typée * le nouvel ensemble non-déf * le nouvel env 
  *)
  let rec pInstr env undefSet inst = 
    match inst.i with 
      | Assign(id,exp) -> 
	let e,undefMap = pExpr env undefSet exp in
	let id = idToSast id in
	if id.Sast.typ != e.Sast.t then
	  (raise   (WrongType(e.Sast.p,e.Sast.t,id.Sast.typ)))
	else 
	  ({Sast.posi = posToSast inst.posi; Sast.i = Sast.Assign(id,e)},
	   (Sset.remove id.Sast.id  undefSet),
	   (Smap.add id.Sast.id {Sast.id = id.Sast.id; Sast.typ = id.Sast.typ} env ))
      (*| For(i,ex1,ex2,li) -> *)

      | Decl(id) -> 
	let Array(size) =  id.typ in
	let ret = ({Sast.posi = posToSast inst.posi; i = Sast.Decl(idToSast id)},
		   (Smap.remove id.id undefSet),
		   (Smap.add id.id {Sast.id = id.id ; typ = typToSast id.typ} env))
	in
	ret
      | Assign_i(name, index, exp) -> 
	let e,undefSet = pExpr env undefSet exp in
        assert (Smap.mem name env);
	let id = Smap.find name env in
	if e.Sast.t != Sast.Bool then
	  (raise   (WrongType(e.Sast.p,e.Sast.t,Sast.Bool)))
	else 
	  ({Sast.posi = posToSast inst.posi; Sast.i = Sast.Assign_i(id,index,e)},
	   (Smap.add name ar undefMap),
	   (Smap.add id.Sast.id {Sast.id = id.Sast.id; Sast.typ = id.Sast.typ} env ))
      | Assign_r(name, i1, i2, exp) ->
	let  e,undefMap = pExpr env undefMap exp in
	assert (Smap.mem name env);
	let id = Smap.find name env in
	let ar = Smap.find name undefMap in
	for k = i1 to i2 do
	  ar.(k) <- 1
	done ;
	(* problème if e.Sast.t != Sast.Array (i2 - i1) then 
	  (raise (WrongType(e.Sast.p,e.Sast.t,Sast.Array(i2 -i1))))
	else *)
	  ({Sast.posi = posToSast inst.posi; Sast.i = Sast.Assign_r(id,i1,i2,e)},
	   (Smap.add name ar undefMap),
	   (Smap.add id.Sast.id {Sast.id = id.Sast.id; Sast.typ = id.Sast.typ} env ))
	    
  and pInstrList env undefSet acc = function
  | [] -> (List.rev acc),undefSet,env
  | i::q -> 
    let inst,set,ev = pInstr env undefSet i in
    pInstrList ev set (inst::acc) q
      

end

(* Vérifie la sémantique des portes etconstruit une map les contenant toutes *)
module GatesToSast = struct 

  open Sast 
  open Exceptions
  exception PError of Past.pos * string
  exception PWrongType of Past.pos * Past.types * Past.types

 let posToSast pos = 
    { Sast.line = pos.line; Sast.char_b = pos.char_b ; Sast.char_e = pos.char_e }
    
  (* Renvoit un type de type Sast *)
  let typToSast = function
    | Past.Bool -> Bool
    | Past.Int -> Int
    | Past.Array s-> Array s
      
  (* renvoit un id de type Sast *)
  let idToSast id = 
    { id = id.Past.id ; typ = typToSast id.Past.typ}

  (* Vérfie une porte 
     prend :
     gate : un porte Past
     renvoit :
     une porte Sast *)
  let pGate (gate : Past.gate) =
    
    (* Traduit la liste des entrées 
       en vérifiant qu'il n'y a que des valeurs gauches 
       On garde simplement une liste de valeurs gauches :
       pas besoin d'expressions *)
    let inputsize = ref 0 in
    let checkInputs expr =
      match expr.Past.e with
	| Past.EVar(ident) -> begin
            match ident.Past.typ with
              | Past.Bool -> incr inputsize
              | Past.Array n -> inputsize := !inputsize + n
              | Past.Int -> raise (PWrongType (expr.Past.p,Past.Int,Past.Bool))
          end;
            idToSast ident
	| _ -> raise (PError(expr.Past.p, "Not a left value"))
    in
    let inputs = List.map checkInputs gate.Past.ginputs in
    
    (* Traduit la liste des sorties 
       Une expression de sortie peut être :
       une valeur gauche
       un sous tableau 
       un element d'un tableau 
       Prend : 
       accList : la liste en court de construction
       accSize : la "taille" réelle du tableau de sortie 
       expr : l'expression 
       Renvoit :
       accList * accSize*)
    let checkOutputs env undef (expr : Past.expr) (accList,size) =

      let expr,_ = InstrToSast.pExpr env  undef expr in
      match expr.e with
	| EVar(ident) -> 
	  let incr = match ident.typ with
	    | Int -> (raise (Exceptions.WrongType(posToSast expr.p,Int,Bool)))
	    | Bool -> 1
	    | Array s -> s
	  in
	  ((expr::accList),(size + incr))
	| EArray_r(ident,i1,i2) -> 
	  (expr::accList,(size + (i2 -i1 )))
	| EArray_i(ident,i) ->
	  (expr::accList,(size + 1))
	| _ -> (raise  (Error(expr.p,"Not a left value")))
    in

    (* création d'un env contenant toutes les entrées *)
    let env = List.fold_left (fun env id -> Smap.add id.id id env)  Smap.empty  inputs in
    (* on ajoute  les entrées au undefMap *)
    let undefMap = List.fold_left (fun map id -> 
      let ar =
	match id.typ with
	  | Array s -> Array.make s 1 
	  | Bool -> [|1|]
      in
      Smap.add id.id ar map) Smap.empty inputs in
    
    (*Typage du corps, vérifit que toutes les variables utilisées sont définies.
      Renvoit l'env des variables locales. *)
    let body,undefMap,env = InstrToSast.pInstrList env undefMap [] gate.Past.gbody in
    (* On vérifie que tout ce qui est utilisé est défini *)
    let () = Smap.iter (fun name ar ->
      for i = 0 to (Array.length ar) -1 do
	if ar.(i) > 1 then (raise (Undefined)) ;
      done ;
    ) undefMap 
    in
    let outputs,size = List.fold_right (checkOutputs env undefMap) gate.Past.goutputs ([],0) in
    { gname = gate.Past.gname; genv = env ; ginputs = inputs ; gbody = body; goutputs = outputs ; goutputsize = size ; ginputsize = !inputsize }


    
  (* construit une map de toutes les portes *)
  let rec buildMap_aux gMap = function
    | [] -> gMap
    | a::q -> begin
      if Smap.mem a.gname gMap then pWarning ;
      buildMap_aux (Smap.add a.gname a gMap) q
    end

  let buildMap = buildMap_aux Smap.empty
      
end 
    
module CircuitToSast = struct

  let pCircuit {Past.gates = gates ; } = {
    Sast.gates = GatesToSast.buildMap (List.map GatesToSast.pGate gates) ; 
  }
    
end

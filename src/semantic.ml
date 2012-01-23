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
  let rec pExpr intEnv env undefSet exp = 
    match exp.e with
      | EBconst b ->
	({ Sast.p = posToSast exp.p ; Sast.e = Sast.EBconst(b) ; Sast.t = Sast.Bool },undefSet)
	  
      | EIconst i -> 
	({ Sast.p = posToSast exp.p ; Sast.e = Sast.EIconst(i); Sast.t = Sast.Int},undefSet)

      | EArray_i(name,index) -> 
	let index,undefSet = pExpr intEnv env undefSet index in
	if (index.Sast.t <> Sast.Int) then (raise (WrongType(index.Sast.p,index.Sast.t, Sast.Int)));
	(* on cherche le tableau dans l'env, forcément déclaré *)
        if not (Smap.mem name env) then (raise (Undefined(posToSast exp.p,name)));
	let id = Smap.find name env in	 
	({ Sast.p = posToSast exp.p ; Sast.e = Sast.EArray_i(id,index); Sast.t = Sast.Bool},undefSet)
	  
      | EArray_r(name,i_beg,i_end) ->
	let i_beg,undefSet =  pExpr intEnv env undefSet i_beg in
	if (i_beg.Sast.t <> Sast.Int) then (raise (WrongType(i_beg.Sast.p,i_beg.Sast.t, Sast.Int)));
	let i_end,undefSet = pExpr intEnv env undefSet i_end in
	if (i_end.Sast.t <> Sast.Int) then (raise (WrongType(i_end.Sast.p,i_end.Sast.t, Sast.Int)));
        assert (Smap.mem name env);
	let id = Smap.find name env in
	({ Sast.p = posToSast exp.p; Sast.e = Sast.EArray_r(id,i_beg,i_end); Sast.t = Sast.Array(-1)},undefSet)
	  
      | EVar(id) ->     

	let id,set = 
	  try 
	    ((Smap.find id.Past.id env),undefSet)
	  with Not_found -> 
	    begin
	      if Smap.mem id.Past.id intEnv then
		({ Sast.id = id.Past.id ; Sast.typ = Sast.Int },undefSet)
	      else
		({Sast.id = id.Past.id; typ = Sast.Bool},(Sset.add id.Past.id undefSet))
	    end
	in
	(({Sast.p = posToSast exp.p; Sast.e = Sast.EVar(id) ; Sast.t = id.Sast.typ}),set)
	  
      | EPrefix(p,ex) -> 
	let e,undefSet = pExpr intEnv env undefSet ex in
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
	let e1,undefSet = pExpr intEnv env undefSet ex1 in
	let e2,undefSet = pExpr intEnv env undefSet ex2 in
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
	let e1,undefSet = pExpr intEnv env undefSet ex1 in
	let e2,undefSet = pExpr intEnv env undefSet ex2 in
        let e3,undefSet = pExpr intEnv env undefSet ex3 in

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
	  let e,undefSet = pExpr intEnv env undefSet exp in
	  ((e::el),undefSet )) args ([],undefSet)  in
	  ({Sast.p = posToSast exp.p; Sast.e = Sast.ECall(name,el); Sast.t = Sast.Array(List.length el) },(undefSet))
    



  (* Typage des Instructions 
     Prend :
     env : l'environnement des variables locales déjà définies 
     undefMap : Map des ident utilisés et ou définis 
     inst : l'instruction à traiter
     Renvoit : 
     une instruction typée * le nouvel ensemble non-déf * le nouvel env 
  *)
  let rec pInstr env intEnv undefSet inst = 
    match inst.i with 
      | Assign(id,exp) -> 
	let e,undefSet = pExpr intEnv env undefSet exp in
	let id = idToSast id in
	if id.Sast.typ != e.Sast.t then
	  (raise   (WrongType(e.Sast.p,e.Sast.t,id.Sast.typ)))
	else 
	  let env,intEnv = match id.Sast.typ with 
	    | Sast.Int -> 
	      (match  e.Sast.e with
		| Sast.EIconst (_) ->  (failwith "not a constant" )
		| Sast.EIconst i -> env,(Smap.add id.Sast.id i intEnv) )
	    | _ ->  (Smap.add id.Sast.id {Sast.id = id.Sast.id; Sast.typ = id.Sast.typ} env ),intEnv
	  in
	  ({Sast.posi = posToSast inst.posi; Sast.i = Sast.Assign(id,e)},
	   (Sset.remove id.Sast.id  undefSet),
	   env,intEnv)
      | For(i,i1,i2,li) ->
	if (Smap.mem i env) or (Smap.mem i intEnv) then (failwith "already defined") ;
	let nIntEnv = Smap.add i i1 intEnv in
	let li,undefSet2,nenv,nIntEnv = List.fold_left (fun (la,ua,nea,nia) elt ->
	  let elt,ua,nea,nia = pInstr nea nia ua elt in (elt::la,ua,nea,nia)) ([],Sset.empty,env,nIntEnv) li in
	if not (Sset.is_empty undefSet2) then (failwith "undifined ident in a for") ;
	let _,nenv = Smap.partition (fun k _ -> Smap.mem k env) nenv in
	({Sast.posi = posToSast inst.posi; i = Sast.For(nenv,nIntEnv,i,i1,i2,li)},
	 (undefSet),
	 env,intEnv)
      | Decl(id) -> 
	let Array(size) =  id.typ in
	let ret = ({Sast.posi = posToSast inst.posi; i = Sast.Decl(idToSast id)},
		   (Sset.remove id.id undefSet),
		   (Smap.add id.id {Sast.id = id.id ; typ = typToSast id.typ} env),intEnv)
	in
	ret
      | Assign_i(name, index, exp) -> 
	let index,undefSet = pExpr intEnv env undefSet index in
	if (index.Sast.t <> Sast.Int) then (raise (WrongType(index.Sast.p,index.Sast.t, Sast.Int)));
	let e,undefSet = pExpr intEnv env undefSet exp in
        assert (Smap.mem name env);
	let id = Smap.find name env in
	if e.Sast.t != Sast.Bool then
	  (raise   (WrongType(e.Sast.p,e.Sast.t,Sast.Bool)))
	else 
	  ({Sast.posi = posToSast inst.posi; Sast.i = Sast.Assign_i(id,index,e)},
	    undefSet,
	   (Smap.add id.Sast.id {Sast.id = id.Sast.id; Sast.typ = id.Sast.typ} env ),intEnv)
      | Assign_r(name, i1, i2, exp) ->
	let i1,undefSet = pExpr intEnv env undefSet i1 in
	if (i1.Sast.t <> Sast.Int) then (raise (WrongType(i1.Sast.p,i1.Sast.t, Sast.Int)));
	let i2,undefSet = pExpr intEnv env undefSet i2 in
	if (i2.Sast.t <> Sast.Int) then (raise (WrongType(i2.Sast.p,i2.Sast.t, Sast.Int)));
	let  e,undefSet = pExpr intEnv env undefSet exp in
	assert (Smap.mem name env);
	let id = Smap.find name env in
	(match  e.Sast.t with 
	  | Sast.Array (_) -> 
	    ({Sast.posi = posToSast inst.posi; Sast.i = Sast.Assign_r(id,i1,i2,e)},
	     undefSet,
	     (Smap.add id.Sast.id {Sast.id = id.Sast.id; Sast.typ = id.Sast.typ} env ),intEnv)
	  | _ -> (raise (WrongType(e.Sast.p,e.Sast.t,Sast.Array(-1)))))
      | Lw (name, i1, i2, name_ad, i1_ad, i2_ad) ->
	assert (Smap.mem name env);
        assert (Smap.mem name_ad env);
        Sast.Lw (name, i1, i2, name_ad, i1_ad, i2_ad);
  and pInstrList env intEnv undefSet acc = function
  | [] -> (List.rev acc),undefSet,env,intEnv
  | i::q -> 
    let inst,set,ev,intEnv = pInstr env intEnv undefSet i in
    pInstrList ev intEnv set (inst::acc) q
      

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
    let checkOutputs env intEnv undef (expr : Past.expr) (accList,size) =

      let expr,_ = InstrToSast.pExpr intEnv env  undef expr in
      match expr.e with
	| EVar(ident) -> 
	  let incr = match ident.typ with
	    | Int -> (raise (Exceptions.WrongType(posToSast expr.p,Int,Bool)))
	    | Bool -> 1
	    | Array s -> s
	  in
	  ((expr::accList),(size + incr))
	| EArray_r(ident,i1,i2) -> 
	  let Sast.EIconst i1 = i1.Sast.e in
	  let Sast.EIconst i2 = i2.Sast.e in
	  (expr::accList,(size + (i2 -i1 )))
	| EArray_i(ident,i) ->
	  let Sast.EIconst i = i.Sast.e in
	  (expr::accList,(size + 1))
	| _ -> (raise  (Error(expr.p,"Not a left value")))
    in

    (* création d'un env contenant toutes les entrées *)
    let env = List.fold_left (fun env id -> Smap.add id.id id env)  Smap.empty  inputs in
    (*Typage du corps, vérifit que toutes les variables utilisées sont définies.
      Renvoit l'env des variables locales. *)
    let body,undefSet,env,intEnv = InstrToSast.pInstrList env Smap.empty Sset.empty [] gate.Past.gbody in
    (* On vérifie que tout ce qui est utilisé est défini *)
    if not (Sset.is_empty undefSet) then 
      (raise (Undefined( ({line = 0; char_b = 0; char_e = 0}),"42" ))) ;
    let outputs,size = List.fold_right (checkOutputs env intEnv undefSet) gate.Past.goutputs ([],0) in
    { gname = gate.Past.gname; genv = env ; gintEnv = intEnv;  ginputs = inputs ; gbody = body; goutputs = outputs ; goutputsize = size ; ginputsize = !inputsize }


    
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

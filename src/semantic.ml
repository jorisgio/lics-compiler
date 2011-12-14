(* Analyse sémantique de la syntaxe abstraite 
   On réalise le typage des expressions, et on vérifie que tout les id sont bien définis
   On type ensuite les instructions
   En sortie, on obtient une syntaxe abstraite telle que définie par Sast.*)


open Ast

module Exceptions = struct
  open Sast
  exception Undefined
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
    | Array s-> Sast.Array(s) 
      
  (* renvoit un id de type Sast *)
  let idToSast id = 
    { Sast.id = id.id ; Sast.typ = typToSast id.typ}
      
      
  (* TODO : remplacer undefSet par une map qui contient des infos de position *) 
  (* Type les expressions. 
    Prend :
     env : l'environnement des variables locales déjà définies
     undefSet : l'ensemble des variables utilisées mais non définies
     exp : l'expression
     Renvoit : une expression typée  * l'ensemble des identifiant non définis 
  *)
  let rec pExpr env undefSet exp = 
    match exp.e with 
      | EBoncst b ->
	({ Sast.p = exp.p ; Sast.e = Sast.EBconst(b) ; Sast.t = Sast.Bool },undefSet)
      | EIconst i -> 
	({ Sast.p = exp.p ; Sast.e = Sast.EIconst(i); Sast.t = Sast.Int},undefSet)
      | EString s -> 
	({ Sast.p = Sast.exp.p ; Sast.e = Sast.EString(s); Sast.t = Sast.String},undefSet)
      | EArray_i(id,index) -> 
	({ Sast.p = exp.p ; Sast.e = Sast.EArray_i(id,index); Sast.t = Bool},undefSet)
      | EArray_r(id,i_beg,i_end) ->
	let Array(size) = id.typ in
	if i_beg < 0 or i_end < 0 or i_end < i_beg or i_end >= size then
	  (raise (Error(exp.p,"Bad index"))) 
	else
	  ({ Sast.p = exp.p; Sast.e = Sast.EArray_r(id,i_beg,i_end); Sast.t = Sast.Array(i_end - i_beg)},undefSet)
      | EVar(id) ->     
	let var,set = 
	  try 
	  (Smap.find id env,undefSet)
	  with Not_found -> ({Sast.id = id; Sast.t = Bool},(Sset.add id undefSet))
	in
	({Sast.p = exp.p; Sast.e = Sast.EVar(id) ; Sast.t = var.typ},set)
	  
      | EPrefix(p,ex) -> 
	let e,set = pExpr env undefSet ex in
	let ty,sp =
	  match p with
	    | Minus -> if e.t != Sast.Int then 
		(raise (WrongType(e.p,e.t,Sast.Int)))
	      else 
		Sast.Int,Sast.Minus
	    |  Not  -> if e.t != Sast.Bool then 
		(raise (WrongType(e.p,e.t,Sast.Bool)))
	      else
		Sast.Bool,Sast.Not
	    | Reg -> if e.t != Sast.Bool then
		(raise (WrongType(e.p,e.t,Sast.Bool)))
	      else
		Sast.Bool,Sast.Reg
	in
	({ p = exp.p ; e = EPrefix(p,e); t = ty},set)
      | EInfix(i,ex1,ex2) -> 
	let e1,s1 = pExpr env undefSet ex1 in
	let e2,s2 = pExpr env undefSet ex2 in
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
	if e1.t != ty or e2.t != ty then
	  (raise (WrongType(e.p,e.t,ty)))
	else
	  ({Sast.p = exp.p ; Sast.e = Sast.EInfix(si,e1,e2); Sast.t = ty},(Sset.union s1 s2)}
      | EMux(_,_,_) -> failwith "Not implemented"




  (* Typage des Instructions 
     Prend :
     env : l'environnement des variables locales déjà définies 
     undefSet : l'ensemble des variables locales utilisées mais non définies 
     inst : l'instruction à traiter
     Renvoit : 
     une instruction typée * le nouvel ensemble non-déf * le nouvel env 
  *)
  let rec pInstr env undefSet inst = 
    match inst.i with 
      | Assign(id,exp) -> 
	let e,undefSet = pExp r env undefSet exp in
	let id = idToSast id in
	if id.typ != e.t then
	  (raise   (WrongType(e.pos,e.t,id.typ)))
	else 
	  ({Sast.posi = inst.posi; Sast.i = Assign(id,e)},(Sset.remove id.id undefSet),(Smap.add id.id {Sast.id = id.id; Sast.typ = id.typ} env ))
      | For(i,ex1,ex2,li) ->
	let inst2,undefSet,tmpEnv =
	  match i with
	    | Assign(id,e) -> if id.typ != Int then
		(raise   (WrongType(e.pos,id.typ,Int)))
	      else
		pInstr env undefSet i 
	    | _ -> (raise   (Error(i.posi,"Wrong instruction")))
	in
	let e1,_ = pExpr env undefSet ex1 in
	let e2,_ =  pExpr env undefSet ex2 in
	if e1.t != Sast.Int then (raise (WrongType(e1.pos,e1.t,Sast.Int))) ;
	if e2.t != Sast.Int then (raise (WrongType(e2.pos,e2.t,Sast.Int))) ;
	let li,undef,env = pInstrList env undefSet [] li  in
	if not (Sset.subset undef undefSet) then
	  (raise (Error({Sast.line = 0; Sast.char_b = 0; Sast.char_e = 0},"Use of unitialised value")))
	else
	  ({Sast.posi = inst.posi; Sast.i = Sast.For(inst2,e1,e2,li) },undef,env)
      | Decl(id,exo) -> 
	let ret =
	  match exo with
	    | None -> ({posi = inst.posi; i = Decl(id, None)},(Smap.add id.id {id = id.id ; typ = id.typ; va = None} env),undefSet)
	    | Some e -> 
	      let e,undefSet = pExpr env undefSet e in
	      if e.t != id.typ then 
		(raise (WrongType(e.p,e.t,id.typ)))
	      else
		({posi = inst.posi; i = Decl(id, Some e)},undefSet,(Smap.add id.id { id = id.id; va = Some e} env))
	in
	ret
      | Past.Envir(li) -> 
	let li,undef,env = pInstrList env undefSet [] li in
	if not (Sset.subset undef undefSet) then
	  (raise (Error({line = 0; char_b = 0; char_e = 0},"Use of unitialised value")))
      else
	  ({posi = inst.posi; i = Envir(li)},undef,env)
	    
  and pInstrList env undefSet acc = function
  | [] -> (List.rev acc),undefSet,env
  | i::q -> 
    let inst,set,ev = pInstr env undefSet i in
    pIntrList ev set (inst::acc) 
      

end

  

(* Vérifie la sémantique des portes etconstruit une map les contenant toutes *)
module GatesToSast = struct 
  open Sast 

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
      match expr.e with
	| Past.EVar(ident) -> begin
            match ident.typ with
              | Bool -> incr inputsize
              | Array n -> inputsize := !inputsize + n
              | Int -> raise (WrongType (expr.pos,Int,Bool))
          end;
            identToSast ident
	| _ -> raise (Error(expr.p, "Not a left value"))
    in
    let inputs = List.map checkInputs gate.ginputs in
    
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
    let checkOutputs (accList,accSize) (expr : Past.expr) =
      let expr = pExpr Smap.empty Sset.empty expr in
      match expr.e with
	| EVar(ident) -> 
	  let incr = match ident.typ with
	    | Int -> (raise (WrongType(expr.p,Int,Bool)))
	    | Bool -> 1
	    | Array s -> s
	  in
	  ((expr::accList),(size + incr))
	| EArray_r(ident,i1,i2) -> 
	  (expr::accList,(size + (i2 -i1 )))
	| Earray_i(ident,i) ->
	  (expr::accList,(size + 1))
	| _ -> (raise  (Error(expr.p,"Not a left value")))
    in
    let outputs,size = List.fold_left checkOutputs ([],0) gate.goutputs in
    (* création d'un env contenant toutes les entrées *)
    let env = List.fold_left (fun acc id -> Smap.add id.id id env) inputs Smap.empty in
    (*Typage du corps, vérifit que toutes les variables utilisées sont définies.
      Renvoit l'env des variables locales. *)
    let body,undefSet,env = pInstrList  env Sset.empty  gate.gbody in
    if not (Sset.is_empty undefSet ) then (raise (Error({line = 0; char_b = 0; char_e = 0},"Use of unitialised value"))) ;
    { gname = gate.gname; genv = env ; ginputs = inputs ; gbody = body; goutputs = outputs ; goutputsize = size  }


    
  (* construit une map de toutes les portes *)
  let rec buildMap gMap = function
    | [] -> gMap
    | a::q -> begin
	if Smap.mem a.gname gMap then pWarning ;
      buildMap (Smap.add a.gname a gMap) q
    end
      
end 
    


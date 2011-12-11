(* Analyse sémantique de la syntaxe abstraite *)
open Ast

module Exceptions = struct
  exception Undefined
  exception Error of pos * string
  exception WrongType of pos * types * types 
      
end

(* affiche un warning *)
let pWarning = () 
  
module Gates = struct
    
  open Sast
  open Exceptions
    
  (* construit une map de toutes les portes *)
  let rec buildMap gMap = function
    | [] -> gMap
    | a::q -> begin
      if Smap.mem a.gname gMap then pWarning ;
      buildMap (Smap.add a.gname a gMap) q
    end

  (* TODO : remplacer undefSet par une map qui contient des infos de position *)    
  let rec pExpr env undefSet exp = 
    match exp.e with 
      | Past.EBoncst b ->  ({ p = exp.p ; e = EBconst(b) ; t = Bool },undefSet)
      | Past.EIconst i -> ({ p = exp.p ; e = EIconst(i); t = Int},undefSet)
      | Past.EString s -> ({ p = exp.p ; e = EString(s); t = String},undefSet)
      | Past.EArray_i(id,index) -> ({  p = exp.p ; e = EArray_i(id,index); t = Bool},undefSet)    
      | Past.EArray_r(id,i_beg,i_end) ->
	if i_beg < 0 or i_end < 0 or i_end < i_beg then
	  (raise (Error(exp.p,"Bad index"))) 
	else
	  ({ p = exp.p; e = EArray_r(id,i_beg,i_end); t = Array(i_end - i_beg)},undefSet)
      | Past.EVar(id) ->
	let var,set = 
	  try 
	    (Smap.find id env,undefSet)
	  with Not_found -> ({id = foo; t = Bool},(Sset.add id undefSet))
	in
	({p = exp.p; e = EVar(id) ; t = var.t},set)
      | Past.EPrefix(p,ex) -> 
	let e,set = pExpr env undefSet ex in
	let ty,sp =
	  match p with
	    | Past.Minus -> if e.t != Int then 
		(raise (WrongType(e.p,e.t,Int)))
	      else 
		Int,Minus
	    | Past.Not  -> if e.t != Bool then 
		(raise (WrongType(e.p,e.t,Bool)))
	      else
		Bool,Not
	    | Past.Reg -> if e.t != Bool then
		(raise (WrongType(e.p,e.t,Bool)))
	      else
		Bool,Reg
	in
	({ p = exp.p ; e = EPrefix(p,e); t = ty},set)
      | Past.EInfix(i,ex1,ex2) -> 
	let e1,s1 = pExpr env undefSet ex1 in
	let e2,s2 = pExpr env undefSet ex2 in
	let ty,si =
	  match i with 
	       | Past.Add -> Int,Add
	       | Past.Sub -> Int,Sub
	       | Past.Mul -> Int,Mul
	       | Past.Div -> Int,Div
	       | Past.And -> Bool,And
	       | Past.Or -> Bool,Or
	       | Past.Xor -> Bool,Xor
	in
	if e1.t != ty or e2.t != ty then
	  (raise (WrongType(e.p,e.t,ty)))
	else
	  ({p = exp.p ; e = EInfix(si,e1,e2); t = ty},(Sset.union s1 s2)}
      | EMux(_,_,_) -> failwith "Not implemented"
	
  let rec pInstr env undefSet inst = 
    match inst.i with 
      | Past.Assign(id,exp) -> 
	let e,undefSet = pExp  r env undefSet exp in
	if id.typ != e.t then
	  (raise   (WrongType(e.pos,e.t,id.typ)))
	else 
	  ({posi = inst.posi; i = Assign(id,e)},(Sset.remove id.id undefSet),(Smap.add id.id {id = id.id; typ = id.typ; va = None} env ))
      | Past.For(i,ex1,ex2,li) ->
	let inst2,undefSet,tmpEnv =
	  match i with
	    | Past.Assign(id,e) -> if id.typ != Int then
		(raise   (WrongType(e.pos,id.typ,Int)))
	      else
		pInstr env undefSet i 
	    | _ -> (raise   (Error(i.posi,"Wrong instruction")))
	in
	let e1,_ = pExpr env undefSet ex1 in
	let e2,_ =  pExpr env undefSet ex2 in
	if e1.t != Int then (raise   (WrongType(e1.pos,e1.t,Int))) ;
	if e2.t != Int then (raise   (WrongType(e2.pos,e2.t,Int))) ;
	let li,undef,env = pInstrList env undefSet []  li  in
	if not (Sset.subset undef undefSet) then
	  (raise (Error({line = 0; char_b = 0; char_e = 0},"Use of unitialised value")))
	else
	  ({posi = inst.posi; i = For(inst2,e1,e2,li) },undef,env)
      | Past.Decl(id,exo) -> 
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


  let pGate gate =
    let checkI (acclist,accenv) expr =
      let expr,undef = pExpr accenv Sset.empty expr in
      if not (Sset.is_empty undef) then  (raise (Error({line = 0; char_b = 0; char_e = 0},"Use of unitialised value")));
      let identi =
	match expr.e with
	  | EVar i -> i
	  | EArray_r(i,b,e) -> i
	  |_ -> (raise (Error(expr.p,"42")))
      in
      if Smap.mem identi accenv then (raise (Error(expr.p,"Argument non unique")));
      (expr::acclist),(Smap.add identi.id identi accenv) 
    in
    let checkO env acclist expr =
      let expr,undef = pExpr env Sset.empty expr in
      if not (Sset.is_empty undef) then  (raise (Error({line = 0; char_b = 0; char_e = 0},"Use of unitialised value")));
      let identi =
	match expr.e with
	  | EVar i -> i
	  | EArray_r(i,b,e) -> i
	  |_ -> (raise (Error(expr.p,"42")))
      in
      if not (Smap.mem identi.id env) then (raise (Error(expr.p,"Sortie non définie"))); 
      (identi::acclist)
    in
    let inpList,env = List.fold_left checkI ([],Smap.empty) gate.ginputs in
    let outList = List.fold_left checkO [] gate.goutputs in
    let inpList = List.rev inpList in
    let outList = List.rev outList in
    let instrs,undef,env = pInstrList env Sset.empty [] gate.gbody in
    if not (Sset.is_empty undef) then (raise (Undefined));
    { gname = gate.gname; genv =env; ginputs = inpList;
      goutputs = outList gbody = instrs }
    
  let gAnalysis cir = 
    let gList = List.map pGate cir.gates in
    {blocks = cir.blocks; buildMap Smap.empty gList}
      
 end
  


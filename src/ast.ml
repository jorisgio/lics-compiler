
module Int = struct
  type t = int
  let compare = Pervasives.compare
end
  
module Imap = Map.Make(Int)


module Sset = Set.Make(String)
module Smap = Map.Make(String)

module Past = struct 
(* expressions entières *) 
  type infix = Add | Sub | Mul  | Di   v |  And | Or | Xor

  type prefix = Not | Reg | Minus
   
  type pos = { line : int; char_b : int; char_e : int}

  type types = Int | Array of int
      
  type ident = { id : string; typ : types }

  type expression = 
    | EBconst of bool
    | EIconst of int
    | EArray_i of ident * int
    | EArray_r of ident * int * int
    | EVar of ident
    | EPrefix of prefix * expr
    | EInfix of infix * expr * expr
    | EMux of expr * expr * expr
	
  and expr = { p : pos; e : expression }
	
  type instruction = 
    | Assign of ident * expr
    | For of instr * expr * expr * instr list 
    | Decl of ident * expr option
    | Envir of instr list

  and  instr =  { posi : pos; i : instruction }
      
  
      
  type gate = {
    gname : string ;
    ginputs : expr list ;
    gbody : instr list ;
    goutputs : expr list;
  }
    
  type block = {
    bname : string; 
    bgate_type : string; (* type de la porte *)
    binputs : expr;
}

(* circuit *)
  type circuit = {
    gates : gate list ;
    start : block ;
    blocks : block list ;
}

end

module Sast = struct 
(* expressions entières *) 
  type infix = Add | Sub | Mul  | Div |  And | Or | Xor

  type prefix = Not | Reg | Minus
   
  type pos = { line : int; char_b : int; char_e : int}
  
  type types = Bool | Int | Array of int

  type ident = { id : string; typ : types; va : expr}

  and  expression = 
    | EBconst of bool
    | EIconst of int
    | EArray_i of ident * int
    | EArray_r of ident * int * int
    | EVar of ident
    | EPrefix of prefix * exp
    | EInfix of infix * expr * expr
    | EMux of expr * expr * expr
	
  and expr = { p : pos; e : expression ; t : types }
	
  type instruction = 
    | Assign of ident * expr
    | For of ident * expr * expr * instr List 
    | Decl of ident * expr option
    | Envir of instr List
  
  and  instr =  { posi : pos; i : instruction }

      
  type gate = {
    gname : string ;
    genv : ident Smap.t
    ginputs : string list;
    gbody : instr List ;
    goutputs : string list;
  }
    
  type block = {
    bname : string; 
    bgate_type : string; (* type de la porte *)
    binputs : expr;
}

(* circuit *)
  type circuit = {
    gates : gate Smap.t ;
    blocks : block list ;
}

end
      
  (* fils entre les instances de block, on garde le point de départ (nom du block et numéro de la sortie *)
  type wire = { block_id : string ; out_id : int }

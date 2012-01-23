module Int = struct
  type t = int
  let compare = Pervasives.compare
end
  
module Imap = Map.Make(Int)


module Sset = Set.Make(String)
module Smap = Map.Make(String)

module Past = struct 
(* expressions entières *) 
  type infix = Add | Sub | Mul  | Div | And | Or | Xor

  type prefix = Not | Reg | Minus
   
  type pos = { line : int; char_b : int; char_e : int}

  type types = Int | Array of int | Bool | Undef
      
  type ident = { id : string; typ : types }

  type expression = 
    (* constantes *)
    | EBconst of bool
    | EIconst of int
    (* Élément i du tableau ident *)
    | EArray_i of string * expr
    (* Sous tableau du tableau ident *)
    | EArray_r of string * expr * expr
    (* valeur gauche *)
    | EVar of ident
    (* opérateur préfixe *)
    | EPrefix of prefix * expr
    (* opérateur infixe *)
    | EInfix of infix * expr * expr
    (* Mux *)
    | EMux of expr * expr * expr
    | ECall of string * expr list
    (* Rw *)
    | ERw of expr list
	
  and expr = { p : pos; e : expression }
      
  type instruction = 
    (* Assigne expr à la valeur gauche ident *)
    | Assign of ident * expr
    (* Assigne expr à la case int du tableau string *)
    | Assign_i of string * expr * expr
    (* Assigne expr au sous tableau *)
    | Assign_r of string * expr * expr * expr
    (* Boucle For *)
    | For of string * int * int * instr list 
    (* Déclare un tabeleau ou un entier *)
    | Decl of ident
	

  and  instr =  { posi : pos; i : instruction }
      
  
      
  type gate = {
    gname : string ;
    ginputs : expr list ;
    gbody : instr list ;
    goutputs : expr list;
  }
    
  (* circuit *)
  type circuit = {
    gates : gate list ;   }
      
end

module Sast = struct 
(* expressions entières *) 
  type infix = Add | Sub | Mul | Div | And | Or | Xor

  type prefix = Not | Reg | Minus
   
  type pos = { line : int; char_b : int; char_e : int}
  
  type types = Bool | Int | Array of int | Wire
	   
  type ident = { id : string; typ : types }

 
  and  expression = 
    | EBconst of bool
    | EIconst of int
    | EArray_i of ident * expr
    | EArray_r of ident * expr * expr
    | EVar of ident
    | EPrefix of prefix * expr
    | EInfix of infix * expr * expr
    | EMux of expr * expr * expr
    | ECall of string * expr list 
	
  and expr = { p : pos; e : expression ; t : types }
	
  type instruction = 
    | Assign of ident * expr
    | Assign_i of ident * expr * expr
    | Assign_r of ident * expr * expr * expr
    | For of (ident Smap.t) * (int Smap.t) * string * int * int * instr list 
    | Decl of ident
  
  and  instr =  { posi : pos; i : instruction }
      
  type gate = {
    gname : string ;
    genv : ident Smap.t;
    gintEnv : int Smap.t; 
    ginputs : ident list;
    gbody : instr list ;
    goutputs : expr list;
    (* longeur réelle de la liste des sorties *)
    goutputsize : int;
    ginputsize : int;
  }
    

(* circuit *)
  type circuit = {
    gates : gate Smap.t ;}
end



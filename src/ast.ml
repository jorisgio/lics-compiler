
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

  type types = Int | Array of int | Bool
      
  type ident = { id : string; typ : types }

  type expression = 
    (* constantes *)
    | EBconst of bool
    | EIconst of int
    (* Élément i du tableau ident *)
    | EArray_i of ident * int
    (* Sous tableau du tableau ident *)
    | EArray_r of ident * int * int
    (* valeur gauche *)
    | EVar of ident
    (* opérateur préfixe *)
    | EPrefix of prefix * expr
    (* opérateur infixe *)
    | EInfix of infix * expr * expr
    (* Mux *)
    | EMux of expr * expr * expr
	
  and expr = { p : pos; e : expression }
      
  type instruction = 
    (* Assigne expr à la valeur gauche ident *)
    | Assign of ident * expr
    (* Assigne expr à la case int du tableau string *)
    | Assign_i of string * int * expr      
    (* Boucle For *)
    | For of instr * expr * expr * instr list 
    (* Déclare un tabeleau ou un entier *)
    | Decl of ident
    (* Sous bloc *)
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

  type ident = { id : string; typ : types }

  and  expression = 
    | EBconst of bool
    | EIconst of int
    | EArray_i of ident * int
    | EArray_r of ident * int * int
    | EVar of ident
    | EPrefix of prefix * expr
    | EInfix of infix * expr * expr
    | EMux of expr * expr * expr
	
  and expr = { p : pos; e : expression ; t : types }
	
  type instruction = 
    | Assign of ident * expr
    | For of ident * expr * expr * instr list 
    | Decl of ident
    | Envir of instr list
  
  and  instr =  { posi : pos; i : instruction }

      
  type gate = {
    gname : string ;
    genv : ident Smap.t;
    ginputs : ident list;
    gbody : instr list ;
    goutputs : expr list;
    (* longeur réelle de la liste des sorties *)
    goutputsize : int;
    ginputsize : int;
  }
    
  type block = {
    bname : string; 
    bgate_type : string; (* type de la porte *)
    binputs : expr list;
}

(* circuit *)
  type circuit = {
    gates : gate Smap.t ;
    blocks : block list ;
}

end

(* Ast partiel obtenu après première passe et traitement des blocs *)

module Bast = struct
  include Sast
  type b_block = {
    b_bname : string ;
    b_bgate_type : string ;
    b_binputs : expr list;
    (* map qui à chaque ident associe un tableau de noeuds *)
    b_bvertices : (int array) Smap.t;
  }

  type b_circuit = {
    b_gates : gate Smap.t ;
    b_blocks : b_block list;
    (* map qui à chaque nom de bloc associe un tableau de noeuds 
       l'élément d'index i de ce tableau est le noeud associè  à la iéme sortie du block *)
    b_blocsOutput : (int  array) Smap.t ;
    b_graphe : Graphe.Graphe.t ;
  (* Graphe ne comportant que des noeuds et pas encore les fils qui les relient *)
  }
end
    
      
  (* fils entre les instances de block, on garde le point de départ (nom du block et numéro de la sortie *)
  type wire = { block_id : string ; out_id : int }

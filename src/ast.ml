(* expressions entières *)
type iop = Add | Sub | Mul  | Div 

type int_expr =
    | Econst of int 
    | Evar of string
    | Ebinop of iop * int_expr * int_expr
    

(* expressions booléenes *)    
type lop = And | Or | Xor | Nand 

type prefix = Not | Reg

type const =
    | Cbool of bool
    | Carray of bool array

type call =
    | Aindex of string * int_expr        (* renvoit l'élement du tableau à l'index donné *)
    | Arange of string * int_expr * int_expr  (* renvoit un sous tableau du tableau *)
    
type logical_expr =
    | Bconst of const
    | Bbinop of lop * logical_expr * logical_expr
    | Bvar of string
    | Bprefix of prefix * logical_expr
    | Bmux of logical_expr * logical_expr * logical_expr
    | Bcall of call
    
(* assertions logiques *)

type logical_stmt =
    | Lassign of string * logical_expr
    | Lrcall of string * int                 (* appel recursif *)
    
(* pdeorte *)

type gate = {
    gname : string ;
    ginputs : string list ;
    gparam : int_expr option ;
    gbody : logical_stmt list ;
    goutputs : string list ;
    }
 
(* fils entre les instances de block, on garde le point de départ (nom du block et numéro de la sortie *)
type wire = { block_id : string ; out_id : int }
    
(* block ( instance ) *)    
type block = {
  bname : string; 
  bgate_type : string; (* type de la porte *)
  bparam : int option;
  binputs : wire list;
}

(* circuit *)
type circuit = {
    gates : gate list ;
    start : block ;
    blocks : block list ;
}
 

(* expressions entières *)
type iop = Add | Sub | Mul  | Div 

type int_expr =
    | Econst of int 
    | Evar of string
    | Ebinop of iop * int_expr * int_expr
    

(* expressions booléenes *)    
type lop = And | Or | Xor | Nand

type prefix = Not | Mux

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
    | Bcall of call
    
(* assertions logiques *)

type logical_stmt =
    | Lassign of string * logical_expr
    
(* porte *)

type gate = {
    name : string ;
    inputs : string list ;
    body : logical_stmt list ;
    outputs : string list ;
    }
    
(* block ( instance ) *)    
type block = string (* not implemented, juste une liste de noms *)

(* circuit *)
type circuit = {
    gates : gate list ;
    start : block ;
    blocks : block list ;
    }

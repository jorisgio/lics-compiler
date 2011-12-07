%{
open Ast

let inputToWire b li =
  List.map (fun i -> { out_id = int_of_string i ; block_id = b}) li

%}


%token <int> INT
%token <bool> BOOL
%token <string> IDENT UIDENT
%token DEF END LW RW
%token LPAREN RPAREN LBRACKET RBRACKET IN OUT
%token AND OR XOR NAND NOT REG PLUS MINUS TIMES DIV MUX
%token COMMA SEMICOLON DOTDOT DOT
%token EQUAL LOWER GREATER EOF 

%nonassoc NOT REG
%left OR XOR
%left AND NAND 
%left TIMES DIV
%left PLUS MINUS

%start circuit
%type <Ast.circuit> circuit


%%
(* un circuit contient : un block d'entrée, une liste de définition de portes et une liste des blocks *)
circuit:
	DEF
	gate_types = gate*
        END
	start = block
	blocks = block* EOF
		{ { gates = gate_types ; start = start ; blocks = blocks } }

(* une porte est un identifier, une liste d'entrées, une liste d'assignement et une liste de sorties *)
gate:
	name = UIDENT IN inp = io
	body = logical_statement*
	OUT out = io
		{ { gname = name; ginputs = inp ; gbody = body ; goutputs = out; gparam = None } }

io:
	LPAREN l=separated_list(COMMA , IDENT) RPAREN 	{l}
			
logical_statement:
	id = IDENT EQUAL  e =  logical_expr SEMICOLON	{ Lassign(id,e) } 	(* assigne une variable locale *)
	
logical_expr:
	| c = const	{ Bconst c }
	| id = IDENT	{ Bvar id  }
	| e1 = logical_expr o = lop e2 = logical_expr 	{ Bbinop(o,e1,e2) }
	| p = prefix e1 = logical_expr	{ Bprefix (p,e1) }
	| MUX LPAREN e1 = logical_expr COMMA e2 = logical_expr COMMA e3 = logical_expr RPAREN { Bmux(e1,e2,e3) }
	| LPAREN e1 = logical_expr RPAREN 	{ e1 }
	| v = IDENT LBRACKET idx=int_expr RBRACKET   { Bcall(Aindex(v, idx)) } (* prend un index du tableau *)
	| v = IDENT LBRACKET min=int_expr DOTDOT max=int_expr RBRACKET  { Bcall(Arange(v, min, max))}(*donne un sous tableau *)
;


%inline lop:
	| OR 	{Or}
	| XOR	{Xor}
	| AND   {And}
	| NAND  {Nand}
;

%inline prefix:
	| NOT 	{Not}
	| REG   {Reg}
;

(* une constante est soit un tableau de booléens, soit un booléen *)
const:
	| b = BOOL 	{ Cbool b }
	| LBRACKET l = separated_list(COMMA , BOOL) RBRACKET { Carray (Array.of_list l) }
;


(* expressions entieres *)
int_expr:
        | i = INT   { Econst i }
        | name = IDENT { Evar name }
        | e1 = int_expr o = iop e2 = int_expr { Ebinop(o, e1, e2) }
;
        
%inline iop:
       | PLUS    { Add }
       | MINUS   { Sub }
       | TIMES   { Mul }
       | DIV     { Div }
;

(* les blocks sont les instanciations des portes, pour construire réelement le circuit *)
block:
    gtype = UIDENT id = IDENT IN 
    LPAREN inp =separated_list(COMMA , wire)  RPAREN { { bname = id ; binputs = inp ; bparam = None; bgate_type = gtype } }
    
wire:
    bid = IDENT DOT outid = INT 	{ {block_id = bid; out_id = outid}}

param:
  LOWER exp = int_expr GREATER   { exp }

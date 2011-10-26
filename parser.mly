%{
open Ast
%}


%token <int> INT
%token <bool> BOOL
%token <string> IDENT UIDENT
%token START DEF END RAM ROM
%token LPAREN RPAREN LBRACKET RBRACKET IN OUT
%token AND OR XOR NAND NOT PLUS MINUS TIMES DIV
%token COMMA SEMICOLON DOTDOT
%token EQUAL EOF 

%nonassoc NOT
%left OR XOR
%left AND NAND
%left TIMES DIV
%left PLUS MINUS

%start circuit
%type <Ast.circuit> circuit


%%
(* un circuit contient : un block d'entrée, une liste de définition de portes et une liste des blocks *)
circuit:
	START start = IDENT SEMICOLON
	DEF
	gate_types = gate*
	END
	blocks = block* EOF
		{ { gates = gate_types ; start = start ; blocks = blocks } }

(* une porte est un identifier, une liste d'entrées, une liste d'assignement et une liste de sorties *)
gate:
	name = UIDENT IN inp = io
	body = logical_statement*
	OUT out = io
		{ { name = (String.lowercase name); inputs = inp ; body = body ; outputs = out } }

io:
	LPAREN l=separated_list(COMMA , IDENT) RPAREN 	{l}
			
logical_statement:
	id = IDENT EQUAL  e =  logical_expr SEMICOLON	{ Lassign(id,e) } 	(* assigne une variable locale *)
	
logical_expr:
	| c = const	{ Bconst c }
	| id = IDENT	{ Bvar id  }
	| e1 = logical_expr o = lop e2 = logical_expr 	{ Bbinop(o,e1,e2) }
	| p = prefix e1 = logical_expr	{ Bprefix (p,e1) }
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
     l = IDENT {l}


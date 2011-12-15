%{
open Ast.Past
open Lexing
(* CODE MORT
let inputToWire b li =
  List.map (fun i -> { out_id = int_of_string i ; block_id = b}) li
*)
let position startpos endpos =
    (* actuellement on se prive de certaines infos *)
    { line = startpos.pos_lnum ;
      char_b = startpos.pos_cnum ;
      char_e = endpos.pos_cnum }

%}


%token <int> INT
%token <bool> BOOL
%token <string> IDENT UIDENT
%token DEF END LW RW
%token LPAREN RPAREN LBRACKET RBRACKET IN OUT
%token AND OR XOR NAND NOT REG PLUS MINUS TIMES DIV MUX
%token COMMA SEMICOLON DOTDOT DOT
%token EQUAL EOF 

%nonassoc NOT REG
%left OR XOR
%left AND NAND 
%left TIMES DIV
%left PLUS MINUS

%start circuit
%type <Ast.Past.circuit> circuit


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
	name = UIDENT IN inp = inp
	body = statement*
	OUT out = out
		{ { gname = name; ginputs = inp ; gbody = body ; goutputs = out } }

inp:
	LPAREN l=separated_list(COMMA , invar) RPAREN 	{l}
			
invar:
    | id = IDENT { {p=position $startpos $endpos; e= EVar {id=id;typ=Bool}} }
    | id = IDENT LBRACKET n = INT RBRACKET { {p=position $startpos $endpos; e= EVar {id=id;typ=Array n}} }
    
out:
	LPAREN l=separated_list(COMMA , outvar) RPAREN 	{l}

outvar:
    | id = IDENT { {p=position $startpos $endpos; e= EVar {id=id;typ=Bool}} }
    | id = IDENT LBRACKET n = INT RBRACKET { {p=position $startpos $endpos; e= EArray_i (id,n)} }
    | id = IDENT LBRACKET i1 = INT DOTDOT i2 = INT RBRACKET { {p=position $startpos $endpos; e= EArray_r (id,i1,i2)} }

statement:
    | id = IDENT EQUAL  e =  expr SEMICOLON	{ {posi = position $startpos $endpos; i=Assign({id = id;typ = Bool},e) } } 	(* assigne une variable locale *)
(*    | id = IDENT LBRACKET n = INT RBRACKET EQUAL e = expr SEMICOLON { Assign_i (id,n,e) } (* assigne dans un tableau *)
NOT IMPLEMENTED YET *)
	
expr:
	| c = const	{ {p=position $startpos $endpos; e=EBconst c} }
	| id = IDENT	{ {p=position $startpos $endpos; e=EVar {id=id;typ=Bool}}  }
	| e1 = expr o = lop e2 = expr 	{ {p=position $startpos $endpos;e=EInfix (o,e1,e2)} }
	| p = prefix e1 = expr	{ {p=position $startpos $endpos;e=EPrefix (p,e1) } }
	| MUX LPAREN e1 = expr COMMA e2 = expr COMMA e3 = expr RPAREN { {p=position $startpos $endpos;e=EMux(e1,e2,e3) }}
	| LPAREN e1 = expr RPAREN 	{ e1 }
	| v = IDENT LBRACKET idx=INT RBRACKET   { {p=position $startpos $endpos;e=EArray_i(v,idx) } } (* prend un index du tableau *)
	| v = IDENT LBRACKET min=INT DOTDOT max=INT RBRACKET  { {p=position $startpos $endpos; e=EArray_r(v,min,max)} }(*donne un sous tableau *)
;


%inline lop:
	| OR 	{Or}
	| XOR	{Xor}
	| AND   {And}
        | TIMES {Mul}
        | DIV   {Div}
(*        | NAND  {Nand} NOT IMPLEMENTED YET *)
        | PLUS  {Add}
        | MINUS {Sub}
;

%inline prefix:
	| NOT 	{Not}
	| REG   {Reg}
        | MINUS {Minus}
;

(* une constante est soit un tableau de booléens, soit un booléen *)
const:
	| b = BOOL 	{ b }
(*	| LBRACKET l = separated_list(COMMA , BOOL) RBRACKET { Carray (Array.of_list l) } *)
;

(* les blocks sont les instanciations des portes, pour construire réelement le circuit *)
block:
    gtype = UIDENT id = IDENT IN 
    LPAREN inp =separated_list(COMMA , expr)  RPAREN { { bname = id; bgate_type = gtype; binputs = inp } }
(* CODE MORT
wire:
    bid = IDENT DOT outid = INT 	{ {block_id = bid; out_id = outid}}
*)

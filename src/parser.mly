%{
open Ast.Past
open Lexing

let position startpos endpos =
    (* actuellement on se prive de certaines infos *)
    { line = startpos.pos_lnum ;
      char_b = startpos.pos_cnum ;
      char_e = endpos.pos_cnum }

%}


%token <int> INT
%token <bool> BOOL
%token <string> IDENT UIDENT
%token LW SW
%token LPAREN RPAREN LBRACKET RBRACKET IN OUT BBLOCK EBLOCK
%token AND OR XOR NOT REG PLUS MINUS TIMES DIV MUX
%token COMMA SEMICOLON DOTDOT DOT
%token EQUAL EOF ARRAY FOR

%nonassoc NOT REG
%left OR XOR
%left AND
%left TIMES DIV
%left PLUS MINUS

%start circuit
%type <Ast.Past.circuit> circuit


%%
(* un circuit contient : un block d'entrée, une liste de définition de portes et une liste des blocks *)
circuit: 
gate_types = gate* EOF
    { { gates = gate_types ; } }

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
    | id = IDENT LBRACKET n = expr RBRACKET { {p=position $startpos $endpos; e= EArray_i (id,n)} }
    | id = IDENT LBRACKET i1 = expr DOTDOT i2 = expr RBRACKET { {p=position $startpos $endpos; e= EArray_r (id,i1,i2)} }

statement:
    | id = IDENT EQUAL  e =  expr SEMICOLON	{ {posi = position $startpos $endpos; i=Assign({id = id;typ = Bool},e) } } 	(* assigne une variable locale *)   
    | id = IDENT LBRACKET n = expr RBRACKET EQUAL e = expr SEMICOLON {{posi = position $startpos $endpos; i= Assign_i (id,n,e)} } (* assigne dans un tableau *)
    | ARRAY id = IDENT LBRACKET n = INT RBRACKET  SEMICOLON { {posi = position $startpos $endpos; i= Decl({id = id ; typ = Array n})}}    (* déclare un tableau *)
    | id = IDENT LBRACKET i1 = expr DOTDOT  i2 = expr RBRACKET EQUAL e = expr SEMICOLON{ {posi = position $startpos $endpos; i=Assign_r(id, i1, i2, e)}}
    | FOR LPAREN s = IDENT COMMA i = INT COMMA i2 = INT RPAREN BBLOCK li = statement* EBLOCK {{posi = position $startpos $endpos; i = For(s,i,i2,li)}}
    | id = IDENT LBRACKET i1 = expr DOTDOT i2 = expr RBRACKET EQUAL LW LPAREN id_a = IDENT LBRACKET i1_a = INT DOTDOT i2_a = INT RBRACKET RPAREN SEMICOLON
    { match i1, i2 with
      | { e = EIconst i1 } , { e = EIconst i2 } ->
      { posi = position $startpos $endpos ; i = Lw (id, i1, i2, id_a, i1_a, i2_a) } 
      | _ -> failwith "Erreur LW : constantes entières attendues"
}
    

	
expr:
    | c = const	{ {p=position $startpos $endpos; e= c} }
    | id = IDENT	{ {p=position $startpos $endpos; e=EVar({id = id; typ = Bool}) } }
    | e1 = expr o = lop e2 = expr 	{ {p=position $startpos $endpos;e=EInfix (o,e1,e2)} }
    | p = prefix e1 = expr	{ {p=position $startpos $endpos;e=EPrefix (p,e1) } }
    | MUX LPAREN e1 = expr COMMA e2 = expr COMMA e3 = expr RPAREN { {p=position $startpos $endpos;e=EMux(e1,e2,e3) }}
    | LPAREN e1 = expr RPAREN 	{ e1 }
    | v = IDENT LBRACKET idx=expr RBRACKET   { {p=position $startpos $endpos;e=EArray_i(v,idx) } } (* prend un index du tableau *)
    | v = IDENT LBRACKET min=expr DOTDOT max=expr RBRACKET  { {p=position $startpos $endpos; e=EArray_r(v,min,max)} }(*donne un sous tableau *)
    | v = UIDENT IN inp = inpcall    { {p=position $startpos $endpos; e= ECall(v,inp)}}
;
inpcall:
  LPAREN l=separated_list(COMMA , expr) RPAREN {l}
;


%inline lop:
	| OR 	{Or}
	| XOR	{Xor}
	| AND   {And}
        | TIMES {Mul}
        | DIV   {Div}
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
	| b = BOOL 	{ EBconst b }
	| i = INT       { EIconst i }
(*	| LBRACKET l = separated_list(COMMA , BOOL) RBRACKET { Carray (Array.of_list l) } *)
;


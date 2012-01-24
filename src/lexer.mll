{
open Lexing
open Parser

exception Lexer_error of string

(* attention, tout les mots clés sont en minuscule ! *)
let kwd_tbl = 
        [ "and", AND; "or", OR; "xor", XOR; "not", NOT; "mux", MUX;
          "reg", REG; "array", ARRAY; "for", FOR;
          "lw", LW; "sw", SW ]
               

let  is_keyword = 
   let h = Hashtbl.create 17 in
   List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
   (fun s -> let s = String.lowercase s in 
	     try Hashtbl.find h s  with Not_found -> IDENT s)
   
let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }


}

let newl = ['\n' '\r' ]
let space = ['\t' ' ' ]
let digit = [ '0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let lowerletter = [ 'a'-'z' ]
let ident = lowerletter+ (digit | letter)*


rule token = parse
        | newl       {newline lexbuf; token lexbuf}
        | space+  { token lexbuf }
        | ("true" | "false") as b { BOOL (bool_of_string b) }
        | ident as id { is_keyword id}
        (* on traite les identifiants commençant par une majuscule *)
        | (['A'-'Z']+ (letter | digit)*) as id {UIDENT id }
        | digit+ as i { INT (int_of_string i) }
        | '='     { EQUAL }
        | '+'     { PLUS }
        | '-'     { MINUS }
        | '/'     { DIV }
        | '*'     { TIMES }
        | '('     { LPAREN }
        | ')'     { RPAREN }
        | ".."    { DOTDOT }
	| '.'     { DOT }
        | ','     { COMMA }
        | ';'     { SEMICOLON }
        | '['     { LBRACKET }
        | ']'     { RBRACKET }
        | "&&"    { AND }
        | "||"    { OR }
        | "->"    { OUT }
        | "<-"    { IN  }
	| '{'     { BBLOCK }
	| '}'     { EBLOCK }
        | "/*"    { comment lexbuf }
        | eof     { EOF }
        | _ as c  { raise (Lexer_error ("Illegal character " ^ String.make 1 c)) }
        
and comment = parse
  | "*/"    { token lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexer_error ("unclosed comment")) }
 

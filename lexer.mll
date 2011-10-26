{
open Lexing
open Parser

exception Lexer_error of string

(* attention, tout les mots clés sont en minuscule ! *)
let kwd_tbl = 
        ["start", START; "def", DEF; "end", END;
               "ram", RAM; "rom", ROM;
               "and", AND; "nand", NAND; "or", OR; "xor", XOR; "not", NOT;
               ]
               

let  is_keyword = 
   let h = Hashtbl.create 17 in
   List.iter (fun (s,t) -> Hashtbl.add h s t) kwd_tbl;
   (fun s -> try Hashtbl.find h s  with Not_found -> IDENT s)
   
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
let ident = lowerletter+ digit*


rule token = parse
        | newl       {newline lexbuf; token lexbuf}
        | space+  { token lexbuf }
        | ("true" | "false") as b { BOOL (bool_of_string b) }
        | ident as id { is_keyword id}
        (* on traite les identifiants commençant par une majuscule *)
        | (['A'-'Z']+ letter* digit*) as id {UIDENT id }
        | digit+ as i { INT (int_of_string i) }
        | '='     { EQUAL }
        | '+'     { PLUS }
        | '-'     { MINUS }
        | '/'     { DIV }
        | '*'     { TIMES }
        | '('     { LPAREN }
        | ')'     { RPAREN }
        | ".."    { DOTDOT }
        | ','     { COMMA }
        | ';'     { SEMICOLON }
        | '['     { LBRACKET }
        | ']'     { RBRACKET }
        | "&&"    { AND }
        | "||"    { OR }
        | "->"    { OUT }
        | "<-"    { IN }
        | "(*"    { comment lexbuf }
        | eof     { EOF }
        | _ as c  { raise (Lexer_error ("Illegal character " ^ String.make 1 c)) }
        
and comment = parse
  | "*)"    { token lexbuf }
  | _       { comment lexbuf }
  | eof     { raise (Lexer_error ("unclosed comment")) }
 

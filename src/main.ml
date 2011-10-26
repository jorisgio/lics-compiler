open Lexing
open Format

let ifile = ref ""
let ofile = ref ""

let set_file f s = f := s

let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

let () =
        ifile := "test.scd";
        if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;
        let f = open_in !ifile in
        let buf = Lexing.from_channel f in
        try     
           let p = Parser.circuit Lexer.token buf in
           close_in f;
         with 
                | Lexer.Lexer_error c ->
                        localisation (Lexing.lexeme_start_p buf);
                        eprintf "Erreur dans l'analyse lexicale %s @" c; exit 1
                | Parser.Error ->
                        localisation (Lexing.lexeme_start_p buf);
                        eprintf "Erreur dans l'analyse syntaxique@."; exit 1
                

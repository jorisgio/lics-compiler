open Lexing
open Format
open Graphe

let set_file f s = f := s

(* fichiers d'entrée et de sortie, options par défaut *)
let ifile = ref ""
let ofile = ref ""
let debug = ref false
let parse_only = ref false
let obj = ref false

(* affiche des messages d'erreur en cas de besoin *)
let deb str = if !debug then Printf.eprintf str

(* liste des options du compilateur *)
let options = [ "-parse-only", Arg.Set parse_only, "N'effectue que le parsage";
		"-g", Arg.Set debug, "Active les options de debogage";
		"-o", Arg.Set_string ofile, "Nom du fichier de sortie";
		"-obj", Arg.Set obj, "La sortie est un fichier contenant des objets ocaml";
	      ]

(* Affiche la localisation dans le buffer lors de l'analyse lexicale/syntaxique *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

 
(* Création d'un tampon d'Analyse Lexicale *)
let circuit =
  let usage = Printf.sprintf "Usage : %s [options] <file.scd>" Sys.argv.(0) in
  Arg.parse options (set_file ifile) usage;
  (* S'il n'y a pas d'entrée, on quitte *)
  if !ifile="" then begin eprintf "Aucun fichier à compiler\n@?"; exit 1 end;
  (* Analyse lexicale *)
  deb "Création du tampon d'Analyse lexicale…\n";
  let f = open_in !ifile in
  let buf = Lexing.from_channel f in
  deb "Done.\n" ;
  
  (* Analyse Syntaxique *)      
  try
    deb "Analyse syntaxique…\n";
    let cir = Parser.circuit Lexer.token buf in
    deb "Done.\n";
    cir
  with 
    | Parser.Error ->
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Erreur dans l'analyse syntaxique@."; exit 1 
    | Lexer.Lexer_error c ->
      localisation (Lexing.lexeme_start_p buf);
      eprintf "Erreur dans l'analyse lexicale %s @" c; exit 1
    
(* Si on ne veut que le parsage, on s'arrête là *)
let () = if !parse_only then (deb "Parsage effectué\n" ; exit 0)
    
(* Analyse sémantique *)
let () = if not (Semantic.analyse circuit) then exit 1 ;
  ()
    
  (* Construction du graphe *)
let igraph =
  deb "Construction du graphe de circuit…\n";
  let g = Buildgraph.buildgraph circuit in
  deb "Done.\n";
  if !debug then
    begin
      deb "INFO : Enregistrement du graphe dans graphe.debug\n";
      let f = open_out "graph.debug" in
      Graphe.drawGraph g.igraph f;
      close_out f
    end ;
  g


(* Suppression des registres *)
   (*  not implemented  *)
let combinatoire =
  { cgraph = igraph.igraph; cinputs = igraph.iinputs; coutputs = igraph.ioutputs; cinregs = []; coutregs = []}
  
(* Tri Topologique et production du code *)
let seqlist = 
  let n = Graphe.size combinatoire.cgraph in
  deb "Tri topologique…\n";
  let l =
    Translator.lics_of_combin_graph combinatoire (n+1) 
  in
  deb "Done.\n";
  l

(* écriture dans un fichier (par défaut, stdout) *)
let () =
  let f =
    if String.length !ofile > 0 then
      open_out !ofile 
    else
      Pervasives.stdout 
  in
  if !obj then
    output_value f seqlist 
  else
    List.iter (fun elt -> Printf.fprintf f  "%s" (Translator.stmt_to_string elt ) ) seqlist
  ;
  exit 0

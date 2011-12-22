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
let o1 = ref false

(* affiche des messages d'erreur en cas de besoin *)
let deb str = if !debug then Printf.eprintf str

(* liste des options du compilateur *)
let options = [ "-parse-only", Arg.Set parse_only, "N'effectue que le parsage";
		"-g", Arg.Set debug, "Active les options de debogage";
		"-o", Arg.Set_string ofile, "Nom du fichier de sortie";
		"-obj", Arg.Set obj, "La sortie est un fichier contenant des objets ocaml";
		"-O1", Arg.Set o1, "Active les optimisations du compilateur";
	      ]

(* Affiche la localisation dans le buffer lors de l'analyse lexicale/syntaxique *)
let localisation pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.eprintf "File \"%s\", line %d, characters %d-%d:\n" !ifile l (c-1) c

 
(* Création d'un tampon d'Analyse Lexicale *)
let circuit =
  let usage = Printf.sprintf "Usage : %s [options] <file.scd>" Sys.argv.(0) in
  Arg.parse options (set_file ifile) usage;
  (* S'il n'y a pas d'entrée, on quitte *)
  if !ifile="" then begin Printf.eprintf "Aucun fichier à compiler\n@?"; exit 1 end;
  (* Analyse lexicale *)
  deb "Création du tampon d'Analyse lexicale…\n";
  let f = open_in !ifile in
  let buf = Lexing.from_channel f in
  (*deb "Done.\n" ;*)
  
  (* Analyse Syntaxique *)      
  try
    deb "Analyse syntaxique…\n";
    let cir = Parser.circuit Lexer.token buf in
    deb "Done.\n";
    cir
  with 
    | Parser.Error ->
      localisation (Lexing.lexeme_start_p buf);
      Printf.eprintf "Erreur dans l'analyse syntaxique@."; exit 1 
    | Lexer.Lexer_error c ->
      localisation (Lexing.lexeme_start_p buf);
      Printf.eprintf "Erreur dans l'analyse lexicale %s @" c; exit 1
    
(* Si on ne veut que le parsage, on s'arrête là *)
let () = if !parse_only then (deb "Parsage effectué\n" ; exit 0)
    
(* Analyse sémantique *)
let circuit =
(*  try*)
  deb "Analyse sémantique...\n";
  let cir = Semantic.CircuitToSast.pCircuit circuit in
  deb "Done\n";
  cir
(*  with
    | e -> Printf.eprintf "Erreur dans l'analyse sémantique" ; exit 1
      (* A DETAILLER *)*)
      
    
  (* Construction du graphe *)
let igraph =
  deb "Construction du graphe de circuit…\n";

  deb "Première passe...\n";
  let cir = Pass1.pBloc circuit in
  deb "Done.\n";
  if !debug then
    begin
      deb "INFO : Enregistrement du graphe dans graph1.debug\n";
      let f = open_out "graph1.debug" in
      Graphe.drawGraph cir.Ast.Bast.b_graphe f;
      close_out f
    end ;

  deb "Deuxième passe...\n";
  let g = Pass2.process cir in
  deb "Done.\n";
  (* Attention : on devrait traiter différemment les entrées / sorties
     afin d'obtenir un interm_graph g *)
  if !debug then
    begin
      deb "INFO : Enregistrement du graphe dans graph2.debug\n";
      let f = open_out "graph2.debug" in
      Graphe.drawGraph g.igraph f;
      close_out f
    end ;
  g

(* Suppression des registres *)
let combinatoire =
  deb "Suppression des registres...\n" ;
  let g = Cycles.processRegs igraph in 
  deb "Done.\n" ;
  begin
    deb "INFO : Enregistrement du graphe dans graph3.debug\n";
    let f = open_out "graph3.debug" in
    Graphe.drawGraph g.cgraph f;
    close_out f
  end ;
  g
  
let n = Graphe.size combinatoire.cgraph
(*let () = printf "Max reg = %d\n" n*)
(* Tri Topologique et production du code *)
let seqlist = 
  deb "Tri topologique et production de LICS…\n";
  let l = 
    try Translator.lics_of_combin_graph combinatoire (n + 1)
    with TopoSort.Circuit_combinatoire label -> Printf.eprintf "Erreur dans le tri topologique : circuit combinatoire au niveau d'un %s\n" (Noeud.string_of_label label);
      exit 1
  in
  deb "Done.\n";
  l

(* Allocation des registres (Optimisation) *)
let seqlist = if !o1 then
    begin
      deb "Allocation de registre…\n";
      let l,g = Regalloc.process seqlist in
      if !debug then Regalloc.IGraphe.drawGraph g (open_out "reg.debug") ;
      deb "Done.\n";
      l
    end
  else
    seqlist

 
(* écriture dans un fichier (par défaut, stdout) *)
let () =
  if !obj then
    if String.length !ofile > 0 then
      LicsFileIO.write !ofile
        { LicsAst.numero_var_max = n ;
          nb_reg =
            List.fold_left
              (function n -> function LicsAst.Inputreg _ -> n + 1 | _ -> n)
              0
              seqlist ;
          programme = seqlist }
    else
      failwith "Fournissez un nom de fichier pour enregistrer le code binaire"
  else
    let f =
      if String.length !ofile > 0 then
        open_out !ofile
      else
        Pervasives.stdout 
    in
    List.iter (fun elt -> Printf.fprintf f  "%s" (Translator.stmt_to_string elt ) ) seqlist
    ;
    exit 0

CMO=lexer.cmo parser.cmo interp.cmo main.cmo
GENERATED = lexer.ml parser.ml parser.mli 
BIN=mini-pascal
FLAGS=

all: $(BIN)
	./$(BIN) test.pas

$(BIN):$(CMO)
	ocamlc $(FLAGS) -o $(BIN) graphics.cma $(CMO)

.SUFFIXES: .mli .ml .cmi .cmo .mll .mly  

.mli.cmi:
	ocamlc $(FLAGS) -c  $<

.ml.cmo:
	ocamlc $(FLAGS) -c  $<

.mll.ml:
	ocamllex $<

.mly.ml:
	menhir -v $<

.mly.mli:
	ocamlyacc -v $<
clean:
	rm -f *.cm[io] *.o *~ $(BIN) $(GENERATED) parser.output

.depend depend:$(GENERATED)
	rm -f .depend
	ocamldep *.ml *.mli > .depend

include .depend




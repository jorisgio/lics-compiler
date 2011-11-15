all: native
	
native:
	ocamlbuild $(ARGS)  src/main.native
bytecode:
	ocamlbuild $(ARGS) src/main.byte
debug:
	ocamlbuild $(ARGS) src/main.d.byte

clean-native:
	ocamlbuild $(ARGS) -clean  src/main.native

clean-bytecode:
	ocamlbuild $(ARGS) -clean  src/main.byte

clean-debug:
	ocamlbuild $(ARGS) -clean  src/main.d.byte
graph:
	ocamldoc -dot -I _build/src -I _build/lib  _build/src/*.ml _build/lib/*.mli _build/lib/*.ml 

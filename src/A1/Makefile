default:
	ocamlbuild -use-ocamlfind enigma_test.byte && ./enigma_test.byte
	
check:
	bash checkenv.sh && bash checktypes.sh
	
clean:
	ocamlbuild -clean
	rm -f checktypes.ml
	
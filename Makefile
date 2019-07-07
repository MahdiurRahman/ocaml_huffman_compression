compress: compress.ml
	ocamlopt -o compress compress.ml

clean:
	rm *.cmi *.cmx *.o compress
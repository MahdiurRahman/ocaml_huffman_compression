compress: compress.ml
	ocamlopt -o compress compress.ml

decompress: decompress.ml
	ocamlopt -o decompress decompress.ml

clean:
	rm *.cmi *.cmx *.o compress decompress decompress.ml
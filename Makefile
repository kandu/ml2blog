ml2blog: html.ml blog.ml ml2blog.ml
	ocamlfind ocamlc -g -linkpkg -package unix -package cmdliner -package str -package compiler-libs.toplevel -package batteries -package ok_gcFile html.ml blog.ml ml2blog.ml -o ml2blog

clean:
	rm -f *.cm* ml2blog

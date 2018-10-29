#!/bin/sh

ocamlfind ocamlc -package ctypes -ccopt `cat ../lib/c_flags.txt` -output-obj ../structs/structs_stubgen.c -o structs_stubgen.o

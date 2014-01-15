#!/bin/bash

if [ "$1" = "-client" ]; then
	ocamlfind ocamlc -package unix,lwt,react,lwt.unix,str -linkpkg -o client.out utils.ml client.ml
	exit
fi 

if [ "$1" = "-server" ]; then
	ocamlfind ocamlc -package unix,lwt,react,lwt.unix,str -linkpkg -o server.out utils.ml server.ml
	exit
fi 

echo "Usage : compile.sh [ -server | -client ]"
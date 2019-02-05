#!/usr/bin/env bash

set -e

cd ../

ocamlfind ocamlc -c data.ml

ocamlfind ocamlopt \
    -package oUnit data.ml \
    -linkpkg -g process.ml test/test.ml \
    -o test/test

cd test/

./test

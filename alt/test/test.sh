#!/usr/bin/env bash

set -e

cd ../

for f in data; do
    ocamlfind ocamlc -c $f.ml
done

ocamlfind ocamlopt \
    -package oUnit data.ml \
    -linkpkg -g convert.ml test/test.ml \
    -o test/test

cd test/

./test
rm oUnit-suite*

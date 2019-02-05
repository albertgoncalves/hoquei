#!/usr/bin/env bash

set -e

d="../test"

cd ../src/

for f in data; do
    ocamlfind ocamlc -c $f.ml
done

ocamlfind ocamlopt \
    -package oUnit data.ml \
    -linkpkg -g convert.ml $d/test.ml \
    -o $d/test

cd $d/

./test
rm oUnit-suite*

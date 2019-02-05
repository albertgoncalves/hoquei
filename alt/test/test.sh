#!/usr/bin/env bash

set -e

d="../test"

cd ../src/

ocamlfind ocamlopt \
    -package oUnit data.ml utils.ml \
    -linkpkg -g convert.ml $d/test.ml \
    -o $d/test

cd $d/

./test
rm oUnit-suite*

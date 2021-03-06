#!/usr/bin/env bash

set -e

d="../test"

cd ../src/
ocamlfind ocamlopt \
    -package extlib,oUnit,str data.ml utils.ml \
    -linkpkg -g scrape.ml record.ml $d/test.ml \
    -o $d/test
cd $d/

echo ""
./test
rm oUnit-suite*

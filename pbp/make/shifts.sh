#!/usr/bin/env bash

set -e

cd ../src/

ocamlfind ocamlopt \
    -package yojson \
    -linkpkg -g utils.ml csv.ml json.ml shifts.ml \
    -o shifts

filename="../data/shifts"
./shifts $filename".json" $filename".csv"

cd ../

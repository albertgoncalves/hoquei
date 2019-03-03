#!/usr/bin/env bash

set -e

for ml in src/*ml; do
    ocp-indent -i $ml
done

cd src/

ocamlfind ocamlopt \
    -package yojson \
    -linkpkg -g utils.ml csv.ml json.ml shifts.ml \
    -o shifts

./shifts "../data/shifts.json" "../data/shifts.csv"

cd ../

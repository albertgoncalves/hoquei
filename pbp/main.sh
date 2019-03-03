#!/usr/bin/env bash

set -e

cd src/

ocamlfind ocamlopt \
    -package yojson \
    -linkpkg -g utils.ml jsonutils.ml shifts.ml \
    -o shifts

./shifts "../data/shifts.json" "../data/shifts.csv"

cd ../

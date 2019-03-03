#!/usr/bin/env bash

set -e

cd src/

ocamlfind ocamlopt \
    -package yojson \
    -linkpkg -g utils.ml shifts.ml \
    -o shifts

./shifts

cd ../

#!/usr/bin/env bash

set -e

cd ../src/

ocamlfind ocamlopt \
    -package yojson \
    -linkpkg -g utils.ml csv.ml json.ml game.ml \
    -o game

directory="../data"
./game $directory"/game.json" $directory"/events.csv" $directory"/players.csv"

cd ../

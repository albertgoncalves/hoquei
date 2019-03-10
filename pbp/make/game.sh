#!/usr/bin/env bash

set -e

cd ../src/

ocamlfind ocamlopt \
    -package yojson \
    -linkpkg -g utils.ml csv.ml json.ml players.ml events.ml game.ml \
    -o game

directory="../data"
./game \
    $directory"/game.json" \
    $directory"/game.csv" \
    $directory"/events.csv" \
    $directory"/players.csv"

cd ../

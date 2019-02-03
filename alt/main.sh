#!/usr/bin/env bash

set -e

ocamlfind ocamlopt -package extlib,str utils.ml -linkpkg main.ml -o main

fn="index.html"

if [ ! -e $fn ]; then
    curl https://www.hockey-reference.com/leagues/NHL_2019_games.html > $fn
fi

./main $fn

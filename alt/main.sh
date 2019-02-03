#!/usr/bin/env bash

set -e

fn="tmp.html"
if [ ! -e $fn ]; then
    curl https://www.hockey-reference.com/leagues/NHL_2019_games.html > $fn
fi

ocamlfind ocamlopt -package str,extlib -linkpkg main.ml -o main
./main tmp.html

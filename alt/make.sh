#!/usr/bin/env bash

set -e

f="scrape"
html="index.html"

cd src/

ocamlfind ocamlopt -package extlib,str utils.ml -linkpkg $f.ml -o $f

cd ../

if [ ! -e $html ]; then
    curl https://www.hockey-reference.com/leagues/NHL_2019_games.html > $html
fi

./src/$f $html

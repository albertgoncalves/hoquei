#!/usr/bin/env bash

set -e

cd test/
sh test.sh
cd ../

f="main"
html="index.html"

cd src/

ocamlfind ocamlopt \
    -package extlib,str \
    -linkpkg -g utils.ml scrape.ml $f.ml \
    -o $f

cd ../

if [ ! -e $html ]; then
    curl https://www.hockey-reference.com/leagues/NHL_2019_games.html > $html
fi

main="./src/$f $html | tail"
printf '\n%s\n' "$main"
eval $main

#!/usr/bin/env bash

set -e

ml="scrape"
fn="index.html"

ocamlfind ocamlopt -package extlib,str utils.ml -linkpkg $ml.ml -o $ml

if [ ! -e $fn ]; then
    curl https://www.hockey-reference.com/leagues/NHL_2019_games.html > $fn
fi

./$ml $fn

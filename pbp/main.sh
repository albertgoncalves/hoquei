#!/usr/bin/env bash

set -e

ocamlfind ocamlopt \
    -package yojson \
    -linkpkg -g $1.ml \
    -o $1

./$1

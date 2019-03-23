#!/usr/bin/env bash

set -e
f="scalpel.hs"
hlint -c=never $f
hindent --indent-size 4 --sort-imports --line-length 79 $f
runghc $f

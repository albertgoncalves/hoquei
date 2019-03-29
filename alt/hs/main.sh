#!/usr/bin/env bash

set -e
f="scalpel.hs"
hlint -c=never $f
runghc $f

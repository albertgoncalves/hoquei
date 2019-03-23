#!/usr/bin/env bash

f="main.hs"
x=$(hlint -c=never $f)
y=$?
if [[ ! $x = "No hints" ]]; then
    echo $x
fi
if [ $y = 0 ]; then
    runghc $f
fi

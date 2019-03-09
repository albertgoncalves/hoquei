#!/usr/bin/env bash

set -e

cd make/

directory="../data/"
if [ ! -d $directory ]; then
    mkdir $directory
    bash pull.sh
fi

for ml in ../src/*ml; do
    ocp-indent -i $ml
done

for f in shifts game; do
    bash $f.sh
done

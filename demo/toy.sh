#!/usr/bin/env bash

set -e

cd toy/

Rscript data.R

cd ../cmdstan/
make ../toy/model

cd ../toy
report="report.csv"
tmp="tmp.txt"
./model sample \
    random seed=1234 \
    data file="input.data.R" \
    output file=$report
./../cmdstan/bin/stansummary $report > $tmp

grep -v "#" $report > "output.csv"
cat $tmp > $report
rm $tmp

cat $report

if [ ! $(uname -s) = "Darwin" ]; then
    alias open="xdg-open"
fi

Rscript posterior.R
open model.png
open output.png

cd ../

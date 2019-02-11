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
    random seed=123 \
    data file="input.data.R" \
    output file=$report
./../cmdstan/bin/stansummary $report > $tmp

grep -v "#" $report > "output.csv"
cat $tmp > $report
rm $tmp

cat $report

Rscript posterior.R
open Rplots.pdf

cd ../

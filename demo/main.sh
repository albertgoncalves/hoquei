#!/usr/bin/env bash

set -e

cd model/

Rscript data.R

cd ../cmdstan/
make ../model/model

cd ../model
report="report.csv"
tmp="tmp.txt"
./model sample \
    adapt delta=0.89 \
    random seed=123 \
    data file="input.data.R" \
    output file=$report
./../cmdstan/bin/stansummary $report > $tmp

grep -v "#" $report > "output.csv"
cat $tmp > $report
rm $tmp

cat $report

cd ../

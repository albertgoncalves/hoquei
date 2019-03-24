# csv to libsvm

---
[XGBoost](https://github.com/dmlc/xgboost/blob/master/demo/binary_classification/README.md) takes input data in [LibSVM](https://www.csie.ntu.edu.tw/~cjlin/libsvm/) format. This little tool handles conversion from `.csv`.

Requirements
---
 * [Nix](https://nixos.org/nix/)

Quickstart
---
```
$ nix-shell
[nix-shell:~/hoquei/pbp/libsvm]$ csvlook data.csv
| Y | a     | b      | c       | d       | e       | f | g   | h     |
| - | ----- | ------ | ------- | ------- | ------- | - | --- | ----- |
| 1 | 0.0   | 3.0001 | 0.00001 | 0.01    | 4.001   | 4 |     | 0.01  |
| 0 | 1.001 | 0.00   | 0.0     | 1000000 | 100.001 | 4 | 0.0 | 10.01 |
```
```
[nix-shell:~/hoquei/pbp/libsvm]$ ghc src/csv2svm.hs
...
[nix-shell:~/hoquei/pbp/libsvm]$ cat data.csv | src/csv2svm
1 2:3.0001 3:0.00001 4:0.01 5:4.001 6:4.0 8:0.01
0 1:1.001 4:1000000.0 5:100.001 6:4.0 8:10.01
```
Alternative delimiters can be provided via the `-d` flag:
```
[nix-shell:~/hoquei/pbp/libsvm]$ echo "0;0.1;0.2;0.0;0.4" | src/csv2svm -d ";"
0 1:0.1 2:0.2 4:0.4
```
---
`$ ./main data.csv` will wrap this together with some linting via `hlint`; this little script was merely to simplify development.

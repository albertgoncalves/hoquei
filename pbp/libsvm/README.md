# csv to libsvm

---
[XGBoost](https://github.com/dmlc/xgboost/blob/master/demo/binary_classification/README.md) takes input data in [LibSVM](https://www.csie.ntu.edu.tw/~cjlin/libsvm/) format. This little tool handle conversion from `.csv`.

At the moment it's only handling line-by-line string conversion... the *real* tool is coming soon!

Requirements
---
 * [Nix](https://nixos.org/nix/)

Quikstart
---
```
$ ./main "1, 0.0, 3.0001, 0.00001, \ta, 4.001, 4, 0.0, b"
1 2:3.0001 3:0.00001 5:4.001 6:4.0
```

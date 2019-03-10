#!/usr/bin/env python3
# -*- coding: utf-8 -*-A

from pandas import read_csv as pd_read_csv


def pipe(data, *fs):
    for f in fs:
        data = f(data)

    return data


def directory(filename):
    return "/".join(["../data", filename])


def read_csv(filename):
    return pd_read_csv(directory(filename), sep=";")

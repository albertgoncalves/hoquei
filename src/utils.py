#!/usr/bin/env python3
# -*- coding: utf-8 -*-


def seasons():
    return ["regular", "playoffs"]


def filename(season, year):
    return "../data/{}_{}.csv".format(season, year)

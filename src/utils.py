#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pandas as pd


def columns():
    return [ "date"
           , "away_team"
           , "away_goals"
           , "home_team"
           , "home_goals"
           , "ot"
           , "attendance"
           , "game_length"
           , "notes"
           ]


def filename(season_handle):
    return "../data/{}.csv".format(season_handle)


def handle(season, year):
    return "{}_{}".format(season, year)


def load_data(season_handle):
    data = pd.read_csv(filename(season_handle))
    data.columns = columns()
    return data


def seasons():
    return ["regular", "playoffs"]

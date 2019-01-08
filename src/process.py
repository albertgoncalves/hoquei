#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from os.path import isfile

import matplotlib.pyplot as plt
import numpy as np

from utils import handle
from utils import filename
from utils import load_data
from utils import seasons


def remove_ot_goals(data):
    overtime = data.ot.notnull()
    home_win = (data.home_goals > data.away_goals)
    data.loc[(overtime & home_win), "home_goals"] -= 1
    data.loc[(overtime & ~home_win), "away_goals"] -= 1
    return data


def main():
    season = seasons()[1]
    year = 2013
    season_handle = handle(season, year)

    if isfile(filename(season_handle)):
        data = remove_ot_goals(load_data(season_handle))
        data["diff"] = data.home_goals - data.away_goals
        print(data.loc[data.ot.notnull(), "diff"].unique())


if __name__ == "__main__":
    main()

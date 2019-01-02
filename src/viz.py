#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from os.path import isfile

import matplotlib.pyplot as plt
import numpy as np

from utils import handle
from utils import filename
from utils import load_data
from utils import seasons


def unit_norm(arr):
    return (arr - np.mean(arr)) / np.std(arr)


def viz_path(handle):
    return "../viz/{}.png".format(handle)


def viz_season(season_handle):
    data = load_data(season_handle)

    for venue in ["home", "away"]:
        unit_norm(data["{}_goals".format(venue)]).hist()
        plt.savefig(viz_path("{}_{}_goals".format(season_handle, venue)))
        plt.close()

    data["goal_diff"] = data.away_goals.values - data.home_goals.values
    data.goal_diff.hist(bins=data.goal_diff.astype(str).nunique())
    plt.savefig(viz_path("{}_goal_diff".format(season_handle)))
    plt.close()


def main():
    season = seasons()[1]
    year = 2013
    season_handle = handle(season, year)

    if isfile(filename(season_handle)):
        viz_season(season_handle)


if __name__ == "__main__":
    main()

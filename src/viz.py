#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from utils import handle
from utils import load_data


def unit_norm(arr):
    return (arr - np.mean(arr)) / np.std(arr)


def viz_path(handle):
    return "../viz/{}.png".format(handle)


def main():
    season = "regular"
    year = 2016
    season_handle = handle(season, year)
    data = load_data(season_handle)

    for venue in ["home", "away"]:
        unit_norm(data["{}_goals".format(venue)]).hist()
        plt.savefig(viz_path("{}_{}_goals".format(season_handle, venue)))
        plt.close()

    data["goal_diff"] = data.away_goals.values - data.home_goals.values
    data.goal_diff.hist(bins=data.goal_diff.astype(str).nunique())
    plt.savefig(viz_path("{}_goal_diff".format(season_handle)))
    plt.close()


if __name__ == "__main__":
    main()

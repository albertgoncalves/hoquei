#!/usr/bin/env python3
# -*- coding: utf-8 -*-A

import matplotlib.pyplot as plt
from pandas import read_csv as pd_read_csv


def pipe(data, *fs):
    for f in fs:
        data = f(data)
    return data


def directory(filename):
    return "/".join(["../data", filename])


def read_csv(filename):
    return pd_read_csv(directory(filename), sep=";")


def filter_shots(events):
    events.sort_values(by=["period", "second"], inplace=True)
    events["prev_result"] = events.result.shift(1)
    rows = (
        events.result.isin(["Shot", "Missed Shot", "Goal"])
        & (events.period <= 3)
    )
    events = events.loc[rows].copy()
    return events.loc[events.prev_result != "Penalty"].copy()


def combine(game):
    return lambda events: events.merge(game, how="left").copy()


def flip(events):
    rows = \
        [ ((events.venue == "away") & events.period.isin([1, 3]))
        , ((events.venue == "home") & (events.period == 2))
        ]
    for row in rows:
        for column in ["x", "y"]:
            events.loc[row, column] *= -1
    return events.copy()


def plot(events):
    _, ax = plt.subplots()
    ax.scatter(events.x, events.y)
    plt.savefig("shots.png")
    plt.close()


if __name__ == "__main__":
    pipe( read_csv("events.csv")
        , combine(read_csv("game.csv"))
        , filter_shots
        , flip
        , plot
        )

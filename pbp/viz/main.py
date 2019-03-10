#!/usr/bin/env python3
# -*- coding: utf-8 -*-A

import matplotlib.lines as lines
import matplotlib.pyplot as plt
import matplotlib.patches as patches
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


def colors(events):
    cmap = \
        { "Missed Shot": 1.0
        , "Shot": 0.5
        , "Goal": 0.0
        }
    events["color"] = events.result.map(cmap)
    return events.copy()


def rink(ax, goal):
    def away(goal):
        return \
            { "x": (goal["x"] * -1) - goal["length"]
            , "y": goal["y"]
            , "length": goal["length"]
            , "width": goal["width"]
            , "edge": goal["edge"]
            }

    def draw_goal(goal, alpha, home=True):
        if home:
            x = goal["x"]
        else:
            goal = away(goal)
            x = goal["x"] + goal["length"]

        rect = ((goal["x"], goal["y"]), goal["length"], goal["width"])
        ax.add_patch(patches.Rectangle(*rect, alpha=alpha, color="k"))

        line = ([x, x], [goal["edge"] * -1, goal["edge"]])
        ax.add_line(lines.Line2D(*line, alpha=alpha))

    def draw_lines(alpha):
        ax.axhline(0, color="k", alpha=0.1)
        ax.axvline(0, color="red", alpha=alpha)
        for v in [25, -25]:
            ax.axvline(v, color="blue", alpha=alpha)

    def draw_circles(alpha):
        xys = [(69 * x, 22 * y) for x in [-1, 1] for y in [-1, 1]]
        for xy in xys:
            ax.add_patch(patches.Circle(xy, 15, fill=None, alpha=alpha))

    ax.set_aspect("equal")
    ax.set_xlim([-100, 100])
    ax.set_ylim([goal["edge"] * -1, goal["edge"]])

    alpha = 0.2
    for home in [True, False]:
        draw_goal(goal, alpha=alpha, home=home)
    draw_lines(alpha=alpha)
    draw_circles(alpha=alpha)


def plot(events):
    goal = \
        { "length": 2
        , "width": 6
        , "x": 89
        , "y": -3
        , "edge": 45
        }

    _, ax = plt.subplots(figsize=(10, 5))
    ax.scatter(events.x, events.y, c=events.color.values)
    rink(ax, goal)
    plt.tight_layout()
    plt.savefig("shots.png")
    plt.close()


def main():
    pipe( read_csv("events.csv")
        , combine(read_csv("game.csv"))
        , filter_shots
        , flip
        , colors
        , plot
        )


if __name__ == "__main__":
    main()

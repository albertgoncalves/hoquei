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


def combine(b):
    return lambda a: a.merge(b, how="left").copy()


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
    # https://www.color-hex.com/color-palette/74832
    cmap = \
        { "Missed Shot": "#ffd100"
        , "Shot": "#58a291"
        , "Goal": "#2c2d65"
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

    goal = \
        { "length": 2
        , "width": 6
        , "x": 89
        , "y": -3
        , "edge": 45
        }
    alpha = 0.2

    ax.set_aspect("equal")
    ax.set_xlim([-100, 100])
    ax.set_ylim([goal["edge"] * -1, goal["edge"]])

    for home in [True, False]:
        draw_goal(goal, alpha=alpha, home=home)

    draw_lines(alpha=alpha)
    draw_circles(alpha=alpha)


def plot(events):
    _, axs = plt.subplots(3, 1, figsize=(10, 15))
    for i in range(3):
        period = i + 1
        subset = events.loc[events.period == period].copy()
        axs[i].scatter(subset.x, subset.y, c=subset.color.values)
        axs[i].set_title(period)
        rink(axs[i])
    plt.tight_layout()
    plt.savefig("shots.png")
    plt.close()


def goalie(shifts):
    shifts["goalie"] = 0
    shifts.loc[shifts.position == "G", "goalie"] = 1
    return shifts.copy()


def process(shifts):
    columns = \
        [ "game_id"
        , "team_id"
        , "venue"
        , "team_name"
        , "period"
        , "second"
        ]
    params = {"player_id": "nunique", "goalie": "sum"}
    shifts = shifts.groupby(by=columns, as_index=False).agg(params)
    shifts.rename(columns={"player_id": "skaters"}, inplace=True)
    shifts.skaters -= shifts.goalie
    return shifts


def pivot(shifts):
    def slice(shifts, columns, venue):
        subset = shifts.loc[shifts.venue == venue].copy()
        params = \
            { "skaters": "{}_skaters".format(venue)
            , "goalie": "{}_goalie".format(venue)
            }
        subset.rename(columns=params, inplace=True)
        return subset[columns + list(params.values())].copy()

    columns = ["game_id", "period", "second"]
    home = slice(shifts, columns, "home")
    away = slice(shifts, columns, "away")
    return home.merge(away, on=columns, how="outer").copy()


def team_strength(events, special_teams=False):
    rows = (
        (events.home_skaters != 5)
        | (events.home_goalie != 1)
        | (events.away_skaters != 5)
        | (events.away_goalie != 1)
    )
    if special_teams:
        return events.loc[rows].copy()
    else:
        return events.loc[~rows].copy()


def main():
    game = read_csv("game.csv")
    shifts = \
        pipe( read_csv("shifts.csv")
            , combine(game)
            , combine(read_csv("players.csv"))
            , goalie
            , process
            , pivot
            )
    events = \
        pipe( read_csv("events.csv")
            , combine(game)
            , combine(shifts)
            , filter_shots
            , lambda events: team_strength(events, special_teams=False)
            , flip
            , colors
            )
    plot(events)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import matplotlib.lines as lines
import matplotlib.pyplot as plt
import matplotlib.patches as patches

from data import flip, filter_shots, goalie, left_join, process, pivot, \
    team_strength

from utils import pipe, read_csv


def colors(cmap):
    def f(events):
        events["color"] = events.result.map(cmap)
        return events.copy()

    return f


def invert(goal):
    return \
        { "x": (goal["x"] * -1) - goal["length"]
        , "y": goal["y"]
        , "length": goal["length"]
        , "width": goal["width"]
        }


def draw_goal(ax, goal, alpha, attack="right"):
    if attack == "right":
        x = goal["x"]
    elif attack == "left":
        goal = invert(goal)
        x = goal["x"] + goal["length"]
    else:
        error = \
            [ "invalid argument for draw_goal(... attack=...)"
            , "valid arguments: \"left\", \"right\""
            ]
        raise ValueError("\n".join(error))

    rect = ((goal["x"], goal["y"]), goal["length"], goal["width"])
    ax.add_patch(patches.Rectangle(*rect, alpha=alpha, color="k"))
    ax.axvline(x, alpha=alpha)


def draw_lines(ax, lines, alpha):
    ax.axvline(lines["center_x"], color="red", alpha=alpha)
    ax.axhline(lines["center_y"], color="k", alpha=0.1)
    for v in [-1, 1]:
        ax.axvline(lines["blue_x"] * v, color="blue", alpha=alpha)


def draw_circles(ax, circles, alpha):
    xys = \
        [ (circles["x"] * x, circles["y"] * y)
          for x in [-1, 1] for y in [-1, 1]
        ]

    for xy in xys:
        ax.add_patch(patches.Circle( xy
                                   , circles["radius"]
                                   , fill=None
                                   , alpha=alpha
                                   ))


def rink(ax, params):
    alpha = 0.2

    ax.set_aspect("equal")
    ax.set_xlim(params["vantage"]["x_lim"])
    ax.set_ylim(params["vantage"]["y_lim"])
    ax.set_xticks([])
    ax.set_yticks([])

    for attack in ["right", "left"]:
        draw_goal(ax, params["goal"], alpha=alpha, attack=attack)

    draw_circles(ax, params["circles"], alpha=alpha)
    draw_lines(ax, params["lines"], alpha=alpha)


def plot(cmap):
    params = \
        { "goal":
            { "length": 2
            , "width": 6
            , "x": 89
            , "y": -3
            }
        , "circles":
            { "x": 69
            , "y": 22
            , "radius": 15
            }
        , "lines":
            { "center_x": 0
            , "center_y": 0
            , "blue_x": 25
            }
        , "vantage":
            { "x_lim": [-10, 100]
            , "y_lim": [-45, 45]
            }
        }

    y_label = \
        { "labelpad": 15
        , "rotation": 0
        , "size": 20
        , "va": "center"
        }

    def f(events):
        _, axs = plt.subplots(3, 2, figsize=(13, 14))

        for i in range(3):
            period = i + 1
            axs[i, 0].set_ylabel(period, **y_label)

            for j, team in enumerate(events.team_name.unique()):
                rows = (events.period == period) & (events.team_name == team)
                subset = events.loc[rows].copy()

                rink(axs[i, j], params)
                axs[i, j].scatter( subset.x
                                 , subset.y
                                 , c=subset.color.values
                                 , s=100
                                 , edgecolor="k"
                                 , alpha=0.8
                                 )

                if i == 0:
                    axs[i, j].set_title(team, size=15)

        handles = \
            [ lines.Line2D( []
                          , []
                          , color=cmap[key]
                          , label=key
                          , ls="None"
                          , marker="o"
                          , markersize=11
                          , markeredgecolor="k"
                          , alpha=0.8
                          )
              for key in cmap.keys()
            ]

        axs[1, 1].legend( loc="center left"
                        , bbox_to_anchor=(0.99, 0.5)
                        , handles=handles
                        , frameon=False
                        )

        plt.tight_layout()
        plt.savefig("shots.png")
        plt.close()

    return f


def main():
    game = read_csv("game.csv")
    shifts = \
        pipe( read_csv("shifts.csv")
            , left_join(game)
            , left_join(read_csv("players.csv"))
            , goalie
            , process
            , pivot
            )
    # https://www.color-hex.com/color-palette/74832
    cmap = \
        { "Missed Shot": "#ffc92d"
        , "Shot": "#0bc99c"
        , "Goal": "#af4e82"
        }
    pipe( read_csv("events.csv")
        , left_join(game)
        , left_join(shifts)
        , filter_shots
        , team_strength(special_teams=False)
        , flip
        , colors(cmap)
        , plot(cmap)
        )


if __name__ == "__main__":
    main()

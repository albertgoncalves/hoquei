#!/usr/bin/env python3
# -*- coding: utf-8 -*-A

import matplotlib.lines as lines
import matplotlib.pyplot as plt
import matplotlib.patches as patches

from data import combine, flip, filter_shots, goalie, process, pivot, \
    team_strength

from utils import pipe, read_csv


def colors(cmap):
    def f(events):
        events["color"] = events.result.map(cmap)
        return events.copy()

    return f


def rink(ax, home=True):
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

    alpha = 0.2
    goal = \
        { "length": 2
        , "width": 6
        , "x": 89
        , "y": -3
        , "edge": 45
        }

    ax.set_aspect("equal")
    ax.set_xlim([-10, 100])
    ax.set_ylim([goal["edge"] * -1, goal["edge"]])
    ax.set_xticks([])
    ax.set_yticks([])

    for home in [True, False]:
        draw_goal(goal, alpha=alpha, home=home)

    draw_lines(alpha=alpha)
    draw_circles(alpha=alpha)


def plot(cmap):
    def f(events):
        _, axs = plt.subplots(3, 2, figsize=(13, 14))
        y_label = \
            { "labelpad": 15
            , "rotation": 0
            , "size": 20
            , "va": "center"
            }

        for i in range(3):
            for j, team in enumerate(events.team_name.unique()):
                period = i + 1
                rows = (events.period == period) & (events.team_name == team)
                subset = events.loc[rows].copy()
                axs[i, j].scatter( subset.x
                                 , subset.y
                                 , c=subset.color.values
                                 , s=70
                                 , edgecolor="k"
                                 , alpha=0.8
                                 )
                rink(axs[i, j])
                if i == 0:
                    axs[i, j].set_title(team, size=15)
            axs[i, 0].set_ylabel(period, **y_label)

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
            , combine(game)
            , combine(read_csv("players.csv"))
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
        , combine(game)
        , combine(shifts)
        , filter_shots
        , team_strength(special_teams=True)
        , flip
        , colors(cmap)
        , plot(cmap)
        )


if __name__ == "__main__":
    main()

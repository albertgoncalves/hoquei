#!/usr/bin/env python3
# -*- coding: utf-8 -*-


def filter_shots(events):
    events.sort_values(by=["period", "second"], inplace=True)
    events["prev_result"] = events.result.shift(1)

    rows = \
        ( events.result.isin(["Shot", "Missed Shot", "Goal"])
        & (events.period <= 3)
        )

    events = events.loc[rows].copy()
    return events.loc[events.prev_result != "Penalty"].copy()


def left_join(b):
    return lambda a: a.merge(b, how="left").copy()


def flip(events, full=True):
    if full:
        rows = \
            [ ((events.venue == "away") & events.period.isin([1, 3]))
            , ((events.venue == "home") & (events.period == 2))
            ]
    else:
        rows = [(events.period == 2)]

    for row in rows:
        for column in ["x", "y"]:
            events.loc[row, column] *= -1

    if events.x.mean() < 0:
        events.x *= -1
        events.y *= -1

    return events.copy()


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
    return shifts.copy()


def slice(shifts, columns, venue):
    params = \
        { "skaters": "{}_skaters".format(venue)
        , "goalie": "{}_goalie".format(venue)
        }

    subset = shifts.loc[shifts.venue == venue].copy()
    subset.rename(columns=params, inplace=True)
    return subset[columns + list(params.values())].copy()


def pivot(shifts):
    columns = ["game_id", "period", "second"]

    home = slice(shifts, columns, "home")
    away = slice(shifts, columns, "away")
    return home.merge(away, on=columns, how="outer").copy(True)


def team_strength(special_teams=False):
    def f(events):
        rows = \
            ( (events.home_skaters != 5)
            | (events.away_skaters != 5)
            | (events.home_goalie != 1)
            | (events.away_goalie != 1)
            )

        if special_teams:
            return events.loc[rows].copy()
        else:
            return events.loc[~rows].copy()

    return f

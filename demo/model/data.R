#!/usr/bin/env Rscript

source("../utils.R")

teams_to_indices = function(index, teams) {
    return(as.vector(sapply(teams, name_to_index(index))))
}

read_data = function(csvfile) {
    data = read_csv(csvfile)[, 1:5]
    names(data) = c("date", "away", "away_goals", "home", "home_goals")
    return(data)
}

export_stan_data = function(data, datafile, teamsfile) {
    teams_list = invert_list(index_names(c(data$away, data$home)))

    n_teams = length(teams_list)
    n_games = NROW(data)
    n_train = as.integer(n_games * 0.9)
    home = teams_to_indices(teams_list, data$home)
    away = teams_to_indices(teams_list, data$away)
    home_goals = data$home_goals
    away_goals = data$away_goals

    items = c(  "n_teams"
             , "n_games"
             , "n_train"
             , "home"
             , "away"
             , "home_goals"
             , "away_goals"
             )

    dump(items, datafile)
    dump("teams_list", teamsfile)
}

if (sys.nframe() == 0) {
    season = "regular_2018"
    csvfile = sprintf("../data/%s.csv", season)
    datafile = "input.data.R"

    data = read_data(csvfile)
    export_stan_data(data, datafile, teamsfile())
}

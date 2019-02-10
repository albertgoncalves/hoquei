#!/usr/bin/env Rscript

source("../utils.R")

teams_to_indices = function(index, teams) {
    return(as.vector(sapply(teams, name_to_index(index))))
}

read_data = function(csvfile) {
    data = read_csv(csvfile)[, 1:6]
    names(data) = c("date", "away", "away_goals", "home", "home_goals", "ot")
    return(data)
}

adjust_ot = function(data) {
    lambda = function(data, rows, team) {
        column = sprintf("%s_goals", team)

        values = data[, column]
        values[rows] = data[rows, column] - 1

        return(values)
    }

    data$ot = ifelse(data$ot == "", 0, 1)

    ot = data$ot == 1
    home_ot_wins = which(ot & (data$home_goals > data$away_goals))
    away_ot_wins = which(ot & (data$home_goals < data$away_goals))

    data$home_goals_no_ot = lambda(data, home_ot_wins, "home")
    data$away_goals_no_ot = lambda(data, away_ot_wins, "away")

    return(data)
}

export_stan_data = function(data, datafile, teamsfile) {
    teams_list = invert_list(index_names(c(data$away, data$home)))

    n_teams = length(teams_list)
    n_games = NROW(data)
    n_train = as.integer(n_games * 0.15)
    home = teams_to_indices(teams_list, data$home)
    away = teams_to_indices(teams_list, data$away)
    home_goals = data$home_goals
    away_goals = data$away_goals
    home_goals_no_ot = data$home_goals_no_ot
    away_goals_no_ot = data$away_goals_no_ot
    ot_input = data$ot

    sigma_offense_lambda = 0.05
    sigma_defense_lambda = 0.05
    sigma_advantage_lambda = 0.001

    items = c( "n_teams"
             , "n_games"
             , "n_train"
             , "home"
             , "away"
             , "home_goals"
             , "away_goals"
             , "home_goals_no_ot"
             , "away_goals_no_ot"
             , "ot_input"
             , "sigma_offense_lambda"
             , "sigma_defense_lambda"
             , "sigma_advantage_lambda"
             )

    dump(items, datafile)
    dump("teams_list", teamsfile)
}

if (sys.nframe() == 0) {
    season = "regular_2018"
    csvfile = sprintf("../data/%s.csv", season)
    datafile = "input.data.R"

    data = adjust_ot(read_data(csvfile))
    export_stan_data(data, datafile, teamsfile())
}

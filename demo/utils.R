#!/usr/bin/env Rscript

read_csv = function(fn) {
    return(read.csv(fn, stringsAsFactors=FALSE))
}

read_data = function(csvfile) {
    data = read_csv(csvfile)[, 1:6]
    names(data) = c("date", "away", "away_goals", "home", "home_goals", "ot")
    data$played = 1

    rows = which(is.na(data[, "away_goals"]))
    data[rows, "played"] = 0

    for (column in c("away_goals", "home_goals")) {
        data[rows, column] = 0
    }

    return(data)
}

index_names = function(xs) {
    u = unique(xs)
    n = length(u)
    ys = as.list(sort(u))
    names(ys) = 1:n
    return(ys)
}

invert_list = function(l) {
    j = as.list(names(l))
    names(j) = as.vector(l)
    return(j)
}

name_to_index = function(index) {
    return(function(name) {
        return(as.integer(index[[name]]))
    })
}

filter_columns = function(data, substring) {
    return(data[, grepl(substring, names(data))])
}

teamsfile = function() {
    return("teams.data.R")
}

draw = function(xs) {
    sample(xs, size=2000, replace=TRUE)
}

extract_index = function(x) {
    return(gsub(".*\\.([0-9]+)", "\\1", x))
}

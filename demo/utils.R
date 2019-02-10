#!/usr/bin/env Rscript

read_csv = function(fn) {
    return(read.csv(fn, stringsAsFactors=FALSE))
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

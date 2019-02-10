#!/usr/bin/env Rscript

library(tools)

source("../utils.R")

draw = function(xs) {
    sample(xs, size=2000, replace=TRUE)
}

extract_index = function(x) {
    return(gsub(".*\\.([0-9]+)", "\\1", x))
}

extract_name = function(teams_list) {
    return(function(x) {
        return(teams_list[extract_index(x)])
    })
}

extract_with_labels = function(data, teams_list, header) {
    draws = data.frame(apply(filter_columns(data, header), 2, draw))
    names(draws) = sapply(names(draws), extract_name(invert_list(teams_list)))
    return(draws)
}

stack_columns = function(data) {
    teams = names(data)
    n = length(teams)
    rows = vector("list", n)

    for (i in 1:n) {
        team = teams[i]
        rows[[i]] = data.frame(posterior=as.vector(data[, team]), team=team)
    }

    return(Reduce(rbind, rows, data.frame()))
}

group = function(data, headers_list, group_functions) {
    data = data.frame(do.call(rbind, by(data, headers_list, group_functions)))
    data$team = rownames(data)
    rownames(data) = c()
    return(data)
}

error_plot = function(data, title) {
    group_functions = function(data) {
        return(c( team=unique(data$team)
                , mean=mean(data$posterior)
                , min=min(data$posterior)
                , max=max(data$posterior)
                , lower=quantile(data$posterior, .05)
                , upper=quantile(data$posterior, .95)
                ))
    }

    center = mean(data$posterior)
    data = group(data, list(data$team), group_functions)
    n = NROW(data)

    data = data[order(data$mean), ]

    par(mar=c(3, 11, 2, 2))
    plot( data$mean
        , 1:n
        , yaxt="n"
        , ylab=""
        , xlim=c(min(data$min), max(data$max))
        , main=title
        )
    gray = adjustcolor("gray", 0.45)
    axis(2, at=1:31, tck=1, lty=2, labels=NA, col=gray)
    axis(side=2, at=1:n, labels=data$team, las=2)
    abline(v=center, lty=1, col=gray)
    segments(y0=1:31, x0=data$lower, x1=data$upper)
}

bundle = function(data, header, teams_list) {
    columns = sprintf("%s.", header)
    title = sprintf("%s Coef.", toTitleCase(header))
    data = stack_columns(extract_with_labels(data, teams_list, columns))
    error_plot(data, title)
}

if (sys.nframe() == 0) {
    source(teamsfile())
    data = read_csv("output.csv")
    bundle(data, "defense", teams_list)
}

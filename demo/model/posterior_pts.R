#!/usr/bin/env Rscript

source("../utils.R")
source("posterior_coef.R")

index_input = function(data) {
    data$n = as.integer(rownames(data))
    return(data)
}

index_posterior = function(data) {
    data = data.frame(t(data))
    data$n = as.integer(extract_index(rownames(data)))
    return(data)
}

rbind_reduce = function(xs) {
    return(Reduce(rbind, xs, data.frame()))
}

assemble_inputs = function(input_data, headers) {
    input_slices = vector("list", 2)

    for (i in 1:2) {
        input_header = headers[i]
        input_slice = input_data[, c(input_header, "n")]
        names(input_slice) = ifelse( names(input_slice) == input_header
                                   , "team"
                                   , names(input_slice)
                                   )
        input_slices[[i]] = input_slice
    }

    return(input_slices)
}

assemble_outputs = function(input_slices, output_data, headers, k) {
    rows = vector("list", 2)

    for (i in 1:2) {
        output_header = sprintf("%s_pts", headers[i])
        output_slice = index_posterior(filter_columns( output_data
                                                     , output_header
                                                     ))[, c(1:k, 1001)]

        rows[[i]] = merge(input_slices[[i]], output_slice, by="n", all=TRUE)
    }

    return(rbind_reduce(rows))
}

extract_sims = function(input_data, output_data, k) {
    group_functions = function(k) {
        return(function(data) {
            column = sprintf("X%d", k)
            return(c(team=unique(data$team), pts=sum(data[, column])))
        })
    }

    sims = vector("list", k)
    headers = c("home", "away")

    input_slices = assemble_inputs(input_data, headers)
    output_slices = assemble_outputs(input_slices, output_data, headers, k)

    for (j in 1:k) {
        sim = output_slices[, c("n", "team", sprintf("X%d", j))]
        sims[[j]] = group(sim, list(sim$team), group_functions(j))
    }

    data = rbind_reduce(sims)
    data$pts = as.integer(as.character(data$pts))

    return(data.frame(data))
}

if (sys.nframe() == 0) {
    source(teamsfile())
    season = "regular_2019"
    csvfile = sprintf("../data/%s.csv", season)
    input_data = index_input(read_data(csvfile))
    output_data = read_csv("output.csv")

    for (header in c("offense", "defense")) {
        bundle(output_data, header, teams_list)
    }

    data = extract_sims(input_data, output_data, 1000)
    error_plot(data, "pts", "Point Projections, 2019")
}

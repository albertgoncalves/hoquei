#!/usr/bin/env Rscript

source("../utils.R")

generate_sims = function(n, m, b, sigma) {
    x = seq(-100, 1200, length.out=n)
    y = rnorm(n, mean=(m * x) + b, sd=sigma)
    return(list(x, y))
}

viz = function(x, y, m, b, ns, xs, ys, m_post, b_post, sigma) {
    plot(x, y, col="white")
    points(xs, ys, col=adjustcolor("gray12", 500 / ns), pch=16)
    for (i in samples) {
        abline(a=b_post[i], b=m_post[i], col=adjustcolor("black", 0.045))
    }
    abline(a=b, b=m, col="red")
    points(x, y, col=adjustcolor("coral", 0.85), pch=16)
    points(x, y, col=adjustcolor("white", 0.85))
}

if (sys.nframe() == 0) {
    set.seed(2)

    source("input.data.R") # source model inputs
    source("parameters.data.R")
    output_data = read_csv("output.csv")

    n = 100
    samples = sample(1:NROW(output_data), n, replace=TRUE)

    m_post = output_data$m
    b_post = output_data$b
    sigma = output_data$sigma

    ss = 10000
    s = sample(1:NROW(output_data), ss, replace=TRUE)
    sims = generate_sims(ss, m_post[s], b_post[s], sigma[s])
    xs = sims[[1]]
    ys = sims[[2]]

    viz(x, y, m, b, ss, xs, ys, m_post, b_post, sigma)
}

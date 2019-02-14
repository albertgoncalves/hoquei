#!/usr/bin/env Rscript

source("../utils.R")

generate_sims = function(n, m, b, sigma) {
    x = seq(-100, 1200, length.out=n)
    y = rnorm(n, mean=(m * x) + b, sd=sigma)
    return(list(x, y))
}

plot_curve = function(posterior_samples, init_value, title) {
    plot(density(posterior_samples), main=title)
    abline(v=init_value, col="red")
}

plot_parameters = function(m, b, sigma, m_post, b_post, sigma_post) {
    plot_curve(m_post, m, "m")
    plot_curve(b_post, b, "b")
    plot_curve(sigma_post, sigma, "sigma")
}

plot_model = function( x, y, m, b, sigma, ns, xs, ys, m_post, b_post
                     , sigma_post) {
    layout(matrix(c(1, 1, 1, 1, 1, 1, 2, 3, 4), 3, 3, byrow=TRUE))

    plot(x, y, col="white")
    points(xs, ys, col=adjustcolor("gray12", 750 / ns), pch=16)

    for (i in samples) {
        abline(a=b_post[i], b=m_post[i], col=adjustcolor("black", 0.055))
    }

    abline(a=b, b=m, col="red")
    points(x, y, col=adjustcolor("coral", 0.85), pch=16)
    points(x, y, col=adjustcolor("white", 0.85))

    plot_parameters(m, b, sigma, m_post, b_post, sigma_post)
}

plot_output = function(output_data) {
    mn = length(names(output_data))
    my = 2
    mx = as.integer(mn / my)
    mx = mx + (mn - (mx * my))

    png("output.png", width=13, height=14, units="in", res=150)

    layout(matrix(1:mn, mx, my, byrow=TRUE))

    for (column in names(output_data)) {
        plot(output_data[, column], type="l", main=column)
    }
}

if (sys.nframe() == 0) {
    set.seed(2)

    source("input.data.R")
    source("parameters.data.R")
    output_data = read_csv("output.csv")

    n = 100
    samples = sample(1:NROW(output_data), n, replace=TRUE)

    m_post = output_data$m
    b_post = output_data$b
    sigma_post = output_data$sigma

    ns = 10000
    s = sample(1:NROW(output_data), ns, replace=TRUE)
    sims = generate_sims(ns, m_post[s], b_post[s], sigma_post[s])
    xs = sims[[1]]
    ys = sims[[2]]

    plot_model(x, y, m, b, sigma, ns, xs, ys, m_post, b_post, sigma_post)
    plot_output(output_data)
}

#!/usr/bin/env Rscript

source("../utils.R")

sim_points = function(n, output_data) {
    m_post = output_data$m
    b_post = output_data$b
    sigma_post = output_data$sigma

    rows = sample(1:NROW(output_data), n, replace=TRUE)
    x = seq(-100, 1200, length.out=n)
    y = rnorm(n, mean=(m_post[rows] * x) + b_post[rows], sd=sigma_post[rows])

    return(list(n, x, y))
}

sim_lines = function(n, output_data) {
    m_post = output_data$m
    b_post = output_data$b

    rows = sample(1:NROW(output_data), n, replace=TRUE)

    return(list(n, m_post[rows], b_post[rows]))
}

plot_curve = function(posterior_samples, init_value, title) {
    plot(density(posterior_samples), main=title)
    abline(v=init_value, col="red")
}

plot_parameters = function(m, b, sigma, output_data) {
    plot_curve(output_data$m, m, "m")
    plot_curve(output_data$b, b, "b")
    plot_curve(output_data$sigma, sigma, "sigma")
}

plot_lines = function(n, m, b, alpha) {
    for (i in 1:n) {
        abline(a=b[i], b=m[i], col=adjustcolor("black", alpha))
    }
}

plot_model = function(x, y, m, b, sigma, output_data, n_lines, n_points) {
    png("model.png", width=6, height=6.5, units="in", res=150)
    layout(matrix(c(1, 1, 1, 1, 1, 1, 2, 3, 4), 3, 3, byrow=TRUE))

    sims = sim_points(n_points, output_data)
    points_alpha = 750 / sims[[1]]
    points_color = adjustcolor("gray12", points_alpha)

    lines = sim_lines(n_lines, output_data)
    lines_alpha = 10 / lines[[1]]

    plot(x, y, col="white")
    points(sims[[2]], sims[[3]], col=points_color, pch=16)
    plot_lines(lines[[1]], lines[[2]], lines[[3]], lines_alpha)

    abline(a=b, b=m, col="red")
    points(x, y, col=adjustcolor("coral", 0.85), pch=16)
    points(x, y, col=adjustcolor("white", 0.85))

    plot_parameters(m, b, sigma, output_data)
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

    plot_model(x, y, m, b, sigma, output_data, 1000, 10000)
    plot_output(output_data)
}

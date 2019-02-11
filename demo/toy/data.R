#!/usr/bin/env Rscript

auto_noise = function(n, y) {
    noise = runif(n, runif(n, -1, 0), runif(n, 0, 1))
    ys = y[sample(1:n, as.integer(n / 3))]
    y = y + (mean(ys) * noise)
    return(y)
}

generate = function() {
    n = 100
    x = sample(1:1000, n, replace=TRUE)
    r = runif(4, -100, 100)
    b = r[1] # | <- explicitly modelled
    m = r[2] # |
    j = r[3] * runif(1, 0.05, 0.125) # | <- hidden, confounding variables
    k = r[4] * runif(1, 0.05, 0.125) # |
    y = auto_noise(n, (m * x) + b) + ((x * j) + k)
    return(list(n, x, y, m, b))
}

if (sys.nframe() == 0) {
    set.seed(8)

    data = generate()
    n = data[[1]]
    x = data[[2]]
    y = data[[3]]
    m = data[[4]]
    b = data[[5]]

    dump(c("n", "x", "y"), "input.data.R")
    dump(c("m", "b"), "parameters.data.R")
}

#!/usr/bin/env Rscript

noise = function(n, y) {
    noise = runif(n, runif(n, -1, 0), runif(n, 0, 1))
    ys = y[sample(1:n, as.integer(n / (length(y) * 0.75)))]
    y = y + (mean(ys) * noise)
    return(y)
}

generate = function() {
    n = 15
    x = sample(1:1000, n, replace=TRUE)
    r = runif(4, -100, 100)
    b = r[1]
    m = r[2]
    y = noise(n, (m * x) + b)
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

#!/usr/bin/env Rscript

generate = function() {
    n = 4
    x = sample(1:100, n, replace=TRUE)
    r = runif(2, -15, 15)
    b = r[1]
    m = r[2]
    sigma = runif(1, 10, 100)
    y = rnorm(n, (m * x) + b, sigma)
    return(list(n, x, y, m, b, sigma))
}

if (sys.nframe() == 0) {
    set.seed(1004)

    data = generate()
    n = data[[1]]
    x = data[[2]]
    y = data[[3]]
    m = data[[4]]
    b = data[[5]]
    sigma = data[[6]]

    dump(c("n", "x", "y"), "input.data.R")
    dump(c("m", "b", "sigma"), "parameters.data.R")
}

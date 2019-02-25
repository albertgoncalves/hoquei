data {
    int<lower=1> n;
    vector[n] x;
    vector[n] y;
}

parameters {
    real m;
    real b;
    real<lower=0.01> sigma;
}

model {
    sigma ~ exponential(0.01);

    target += normal_lpdf(y | (m * x) + b, sigma);  # | equivalent expressions
    # y ~ normal((m * x) + b, sigma);               # |
}

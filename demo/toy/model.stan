data {
    int<lower=1> n;
    real x[n];
    real y[n];
}

parameters {
    real m;
    real b;
    real<lower=0.01> sigma;
}

model {
    vector[n] mu;

    sigma ~ exponential(0.1);

    for (i in 1:n) {
        mu[i] = (m * x[i]) + b;
    }

    y ~ normal(mu, sigma);
}

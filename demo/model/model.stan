data {
    int<lower=1> n_teams;
    int<lower=1> n_games;
    int<lower=1> n_train;
    int<lower=1, upper=n_teams> home[n_games];
    int<lower=1, upper=n_teams> away[n_games];
    int<lower=0> home_goals[n_games];
    int<lower=0> away_goals[n_games];
}

parameters {
    real home_advantage;
    real offense[n_teams];
    real defense[n_teams];

    real<lower=0.01> sigma_advantage;
    real<lower=0.01> sigma_offense;
    real<lower=0.01> sigma_defense;
}

model {
    vector[n_train] home_xg;
    vector[n_train] away_xg;

    sigma_offense ~ exponential(0.1);
    sigma_defense ~ exponential(0.1);
    sigma_advantage ~ exponential(0.01);

    offense ~ normal(0, sigma_offense);
    defense ~ normal(0, sigma_defense);
    home_advantage ~ normal(0, sigma_advantage);

    for (n in 1:n_train) {
        home_xg[n] = (
            offense[home[n]]
            + defense[away[n]]
            + home_advantage
        );
        away_xg[n] = (
            offense[away[n]]
            + defense[home[n]]
        );

        home_goals[n] ~ poisson_log(home_xg[n]);
        away_goals[n] ~ poisson_log(away_xg[n]);
    }
}

generated quantities {
    int<lower=0> pred_home_goals[n_games - n_train];
    int<lower=0> pred_away_goals[n_games - n_train];

    vector[n_games - n_train] home_xg;
    vector[n_games - n_train] away_xg;

    for (n in 1:(n_games - n_train)) {
        home_xg[n] = (
            offense[home[n]]
            + defense[away[n]]
            + home_advantage
        );
        away_xg[n] = (
            offense[away[n]]
            + defense[home[n]]
        );

        pred_home_goals[n] = poisson_rng(exp(home_xg[n]));
        pred_away_goals[n] = poisson_rng(exp(away_xg[n]));
    }
}

data {
    int<lower=1> n_teams;
    int<lower=1> n_games;
    int<lower=1> n_train;
    int<lower=1, upper=n_teams> home[n_games];
    int<lower=1, upper=n_teams> away[n_games];
    int<lower=0> home_goals[n_games];
    int<lower=0> away_goals[n_games];
    int<lower=0> home_goals_no_ot[n_games];
    int<lower=0> away_goals_no_ot[n_games];
    int<lower=0, upper=1> ot_input[n_games];
    real<lower=0.001> sigma_offense_lambda;
    real<lower=0.001> sigma_defense_lambda;
    real<lower=0.001> sigma_advantage_lambda;
}

transformed data {
    int n_pred = n_games - n_train;
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
    sigma_offense ~ exponential(sigma_offense_lambda);
    sigma_defense ~ exponential(sigma_defense_lambda);
    sigma_advantage ~ exponential(sigma_advantage_lambda);

    offense ~ normal(0, sigma_offense);
    defense ~ normal(0, sigma_defense);
    home_advantage ~ normal(0, sigma_advantage);

    for (n in 1:n_train) {
        home_goals_no_ot[n] ~ poisson_log(
            offense[home[n]] + defense[away[n]] + home_advantage
        );
        away_goals_no_ot[n] ~ poisson_log(
            offense[away[n]] + defense[home[n]]
        );
    }
}

generated quantities {
    int<lower=0> pred_home_goals[n_pred];
    int<lower=0> pred_away_goals[n_pred];

    int<lower=0, upper=2> home_pts[n_games];
    int<lower=0, upper=2> away_pts[n_games];
    int<lower=0, upper=1> ot[n_games];

    for (n in 1:n_train) {
        if (ot_input[n] == 0) {
            if (home_goals[n] > away_goals[n]) {
                home_pts[n] = 2;
                away_pts[n] = 0;
                ot[n] = 0;
            } else {
                home_pts[n] = 0;
                away_pts[n] = 2;
                ot[n] = 0;
            }
        } else {
            if (home_goals[n] > away_goals[n]) {
                home_pts[n] = 2;
                away_pts[n] = 1;
                ot[n] = 1;
            } else {
                home_pts[n] = 1;
                away_pts[n] = 2;
                ot[n] = 1;
            }
        }
    }

    for (n in 1:n_pred) {
        pred_home_goals[n] = poisson_rng(
            exp(
                offense[home[n + n_train]]
                + defense[away[n + n_train]]
                + home_advantage
            )
        );
        pred_away_goals[n] = poisson_rng(
            exp(offense[away[n + n_train]] + defense[home[n + n_train]])
        );

        if (pred_home_goals[n] > pred_away_goals[n]) {
            home_pts[n + n_train] = 2;
            away_pts[n + n_train] = 0;
            ot[n + n_train] = 0;
        } else if (pred_home_goals[n] < pred_away_goals[n]) {
            home_pts[n + n_train] = 0;
            away_pts[n + n_train] = 2;
            ot[n + n_train] = 0;
        } else {
            if (uniform_rng(0.0, 1.0) >= 0.5) {
                home_pts[n + n_train] = 2;
                away_pts[n + n_train] = 1;
                ot[n + n_train] = 1;
            } else {
                home_pts[n + n_train] = 1;
                away_pts[n + n_train] = 2;
                ot[n + n_train] = 1;
            }
        }
    }
}

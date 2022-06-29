data {
  int<lower=1> N;  // total number of observations
  vector[N] Y;  // response variable
  int<lower=1> K;  // number of population-level effects
  matrix[N, K] X;  // population-level design matrix; NO INTERCEPT
  real birders_estimate; // number of birders viewing sea eagle, estimated by birders
}

parameters {
  real alpha;
  vector[K] beta;  // population-level effects
  real<lower=0> sigma;  // residual SD
  vector[N] p_Y; // predicted values
}
transformed parameters {
  vector[N] mu;
  mu = alpha + X * beta;
}

model {
  //priors
  alpha ~ normal(0, 5);
  beta ~ normal(0, 2);
  sigma ~ exponential(1);
  //likelihood
  Y ~ normal(mu, sigma);
  p_Y ~ normal(mu, sigma);
}

generated quantities {
  real mean_cost;
  real total;
  mean_cost = mean(exp(p_Y));
  total = mean_cost * birders_estimate;
}

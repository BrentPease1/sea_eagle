data {
  int<lower = 1> N;
  vector[N] Y;
  vector[N] Y_time; // accoutning for time
  int<lower = 1> K;
  matrix[N, K] X;
  real birders_estimate_mean;
  real birders_estimate_max;
  real ebird_estimate;
  real twitter_estimate;
}

transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc;  // centered version of X without an intercept
  vector[Kc] means_X;  // column means of X before centering
  for (i in 2:K) {
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}

parameters {
  vector[Kc] b;  // population-level effects
  vector[Kc] b_time;
  real Intercept;  // temporary intercept for centered predictors
  real Intercept_time;
  real<lower=0> sigma;  // residual SD
  real<lower = 0> sigma_time;
}

transformed parameters {
}

model {
  Y ~ normal(Xc*b + Intercept, sigma);
  Y_time ~ normal(Xc*b_time + Intercept_time, sigma_time);

  b ~ normal(0, 2);
  Intercept ~ normal(0, 5);
  sigma ~ exponential(1);
  
  b_time ~ normal(0, 2);
  Intercept_time ~ normal(0, 5);
  sigma_time ~ exponential(1);

}

generated quantities {
  real mean_cost;
  real mean_cost_time;
  vector[N] p_Y;
  vector[N] p_Y_time;
  
  real total_birders_mean;
  real total_birders_mean_time;
  
  real total_birders_max;
  real total_birders_max_time;
  
  real total_ebird;
  real total_ebird_time;
  
  real total_twitter;
  real total_twitter_time;
  
  p_Y = Xc*b + Intercept;
  p_Y_time = Xc * b_time + Intercept_time;

  
  mean_cost = mean(exp(p_Y) - 1);
  mean_cost_time = mean(exp(p_Y_time) - 1);
  
  total_birders_mean = mean_cost * birders_estimate_mean;
  total_birders_mean_time = mean_cost_time * birders_estimate_mean;
  
  total_birders_max = mean_cost * birders_estimate_max;
  total_birders_max_time = mean_cost_time * birders_estimate_max;
  
  total_ebird = mean_cost * ebird_estimate;
  total_ebird_time = mean_cost_time * ebird_estimate;
  
  total_twitter = mean_cost * twitter_estimate;
  total_twitter_time = mean_cost_time * twitter_estimate;
}
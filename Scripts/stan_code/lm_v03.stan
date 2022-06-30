functions {
  /* compute monotonic effects
   * Args:
   *   scale: a simplex parameter
   *   i: index to sum over the simplex
   * Returns:
   *   a scalar between 0 and 1
   */
  real mo(vector scale, int i) {
    if (i == 0) {
      return 0;
    } else {
      return rows(scale) * sum(scale[1:i]);
    }
  }
}

data {
  int<lower = 1> N; // number of observations
  vector[N] Y1; // estimated expenditure without time
  vector[N] Y2; // esimated expenditure WITH time/opportunity cost
  int<lower = 1> K; // number of population-level effects
  matrix[N, K] X; // population level design matrix
  int<lower = 1> Ksp; // number of special effects terms
  int<lower = 1> Imo;  // number of monotonic variables
  int<lower = 1> Jmo; // length of simplexes
  int Xmo_1[N]; // monotonic variable
  vector[5] con_simo_1; // prior concentration of monotonic simplex
  real birders_estimate_mean;
  real birders_estimate_max;
  real ebird_estimate;
  real twitter_estimate;
}

transformed data {
  int Kc = K - 1;
  matrix[N, Kc] Xc; // centered version of X without an intercept
  vector[Kc] means_X; // column means of X before centering
  for(i in 2:K){
    means_X[i - 1] = mean(X[, i]);
    Xc[, i - 1] = X[, i] - means_X[i - 1];
  }
}

parameters {
  vector[Kc] b1; // betas for no-time estimates
  vector[Kc] b2; // betas for time estimates
  real Intercept1; 
  real Intercept2;
  vector[Ksp] bsp1; // special effects coefficients
  vector[Ksp] bsp2;
  simplex[5] simo_11;
  simplex[5] simo_12;
  real<lower = 0> sigma1;
  real<lower = 0> sigma2;
}

transformed parameters {
  vector[N] mu1 = Intercept1 + rep_vector(0.0, N);
  vector[N] mu2 = Intercept2 + rep_vector(0.0, N);
  for(n in 1:N){
    mu1[n] += (bsp1[1]) * mo(simo_11, Xmo_1[n]);
    mu2[n] += (bsp2[1]) * mo(simo_12, Xmo_1[n]);
  }
}

model {
  //vector[N] mu1 = Intercept1 + rep_vector(0.0, N);
  //vector[N] mu2 = Intercept2 + rep_vector(0.0, N);
  // for(n in 1:N){
  //   mu1[n] += (bsp1[1]) * mo(simo_11, Xmo_1[n]);
  //   mu2[n] += (bsp2[1]) * mo(simo_12, Xmo_1[n]);
  // }
  target += normal_id_glm_lpdf(Y1 | Xc, mu1, b1, sigma1);
  target += normal_id_glm_lpdf(Y2 | Xc, mu2, b2, sigma2);
  //priors
  target += normal_lpdf(Intercept1 | 0, 5);
  target += normal_lpdf(Intercept2 | 0, 5);
  target += normal_lpdf(b1 | 0, 2);
  target += normal_lpdf(b2 | 0, 2);
  target += dirichlet_lpdf(simo_11 | con_simo_1);
  target += dirichlet_lpdf(simo_12 | con_simo_1);
  target += exponential_lpdf(sigma1 | 1);
  target += exponential_lpdf(sigma2 | 1);
}

generated quantities {
  real mean_cost1; // average per capita expenditure, not accounting for time
  real mean_cost2; // average per capita expenditure with time loss
  vector[N] pY1; // fitted values, time NOT included
  vector[N] pY2; // fitted values, time included
  real birders_mean1; // estimated total expendiure: no time, using mean number of observers estimated by birders
  real birders_mean2; // estimated total expendiure: Yes time, using mean number of observers estimated by birders
  real birders_max1; // same as birders_mean1, but using maximum esimated birders on scene
  real birders_max2; // same as birders_mean2, but using maximum esimated birders on scene
  real ebird1; // estimated total expendiure: no time, using number of observers derived from ebird
  real ebird2; // estimated total expendiure: yes time, using number of observers derived from ebird
  real twitter1; // estimated total expendiure: no time, using number of observers derived from twitter
  real twitter2; // estimated total expendiure: yes time, using number of observers derived from twitter
  
  pY1 = Xc*b1 + mu1; // fitted values (no time)
  pY2 = Xc*b2 + mu2; // fitted values (time)
  
  mean_cost1 = mean(exp(pY1) - 1); // average of fitted values (no time), transformed to dollar scale
  mean_cost2 = mean(exp(pY2) - 1); // average of fitted values (time), transformed to dollar scale
  
  birders_mean1 = mean_cost1 * birders_estimate_mean;
  birders_mean2 = mean_cost2 * birders_estimate_mean;
  birders_max1 = mean_cost1 * birders_estimate_max;
  birders_max2 = mean_cost2 * birders_estimate_max;
  ebird1 = mean_cost1 * ebird_estimate;
  ebird2 = mean_cost2 * ebird_estimate;
  twitter1 = mean_cost1 * twitter_estimate;
  twitter2 = mean_cost2 * twitter_estimate;
}
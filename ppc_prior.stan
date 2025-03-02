
data {
  int<lower=0> N;  // Number of observations
}

parameters {
  real alpha;  // Intercept
  real beta_vix;  //  VIX previous day
  real beta_cpi;  //CPI Year-over-Year
  real beta_pce;  // PCE Year-over-Year
  real beta_forcast;  // Forcast Change
   real<lower=0> sigma;  // Error
}

model {
  // Priors
  target += normal_lpdf(alpha | 3, 5);
  target += normal_lpdf(beta_vix | 1, 13);
  target += normal_lpdf(beta_cpi | 2.5, 2);
  target += normal_lpdf(beta_pce | 2.5, 3);
  target += normal_lpdf(beta_forcast | 0, 0.25);
  target += exponential_lpdf(sigma | 0.6);
}

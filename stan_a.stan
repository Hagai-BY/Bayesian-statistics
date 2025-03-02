data {
  int<lower=0> N;  // Number of observations
  vector[N] y;  // daily return
  vector[N] vix_prev_day; 
  vector[N] cpi_yoy; 
  vector[N] gdp_yoy;
  vector[N] pce_yoy;
  vector[N] un_rate;
  vector[N] forcast_change;
}

parameters {
  real alpha;  // Intercept
  real beta_vix;  //  VIX previous day
  real beta_cpi;  //  CPI Year-over-Year
  real beta_gdp;  //  GDP Year-over-Year
  real beta_pce;  //  PCE Year-over-Year
  real beta_un;  //Unemployment Rate
  real beta_forcast;  // Forcast Change
  real<lower=0> sigma;  // SD
}

model {
  // Priors
  target += normal_lpdf(alpha | 3, 4);
  target += normal_lpdf(beta_vix | 1, 13);
  target += normal_lpdf(beta_cpi | 2.5, 2);
  target += normal_lpdf(beta_gdp | 2, 2.5);
  target += normal_lpdf(beta_pce | 2.5, 3);
  target += normal_lpdf(beta_un | 5, 4);
  target += normal_lpdf(beta_forcast | 0, 0.25);
  target += exponential_lpdf(sigma | 0.6);
  
  // Likelihood
  y ~ normal(alpha + beta_vix * vix_prev_day +
             beta_cpi * cpi_yoy +
             beta_gdp * gdp_yoy +
             beta_pce * pce_yoy +
             beta_un * un_rate +
             beta_forcast * forcast_change, sigma);
}
// Log-likelihood

generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;

  for (n in 1:N) {
    y_rep[n] = normal_rng(alpha + beta_vix * vix_prev_day[n] +
                          beta_cpi * cpi_yoy[n] +
                          beta_gdp * gdp_yoy[n] +
                          beta_pce * pce_yoy[n] +
                          beta_un * un_rate[n] +
                          beta_forcast * forcast_change[n], sigma);
    log_lik[n] = normal_lpdf(y[n] | alpha + beta_vix * vix_prev_day[n] +
                                      beta_cpi * cpi_yoy[n] +
                                      beta_gdp * gdp_yoy[n] +
                                      beta_pce * pce_yoy[n] +
                                      beta_un * un_rate[n] +
                                      beta_forcast * forcast_change[n], sigma);
  }
}

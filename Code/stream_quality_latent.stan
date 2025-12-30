// Bayesian Latent Variable Model for Stream Quality
// Based on Scott Brown's ecological condition model
// https://github.com/cbrown5/ecological-condition-latent-model
//
// Model structure:
// - Latent variable "stream_quality" (nu) represents unobserved stream condition
// - 4 environmental indicators: conductivity, max depth, DO, thermal index
// - Each indicator = intercept + loading * stream_quality + error
// - Latent variable follows standard normal: N(0, 1)

data {
  int<lower=1> N;  // Number of observations
  vector[N] conduct_log;  // Log conductivity (scaled)
  vector[N] max_depth;    // Max depth (scaled)
  vector[N] dissolved_oxygen;  // Dissolved oxygen (scaled)
  vector[N] thermal;      // Thermal index (scaled)
}

parameters {
  // Latent variable: stream quality for each observation
  vector[N] stream_quality;

  // Factor loadings (how strongly each indicator reflects stream quality)
  real beta_conduct;
  real<lower=0> beta_depth;  // Constrained positive (deeper = better quality)
  real<lower=0> beta_do;     // Constrained positive (higher DO = better quality)
  real beta_thermal;         // Can be negative (higher thermal = worse quality)

  // Intercepts for each indicator
  real a_conduct;
  real a_depth;
  real a_do;
  real a_thermal;

  // Residual standard deviations for each indicator
  real<lower=0> sigma_conduct;
  real<lower=0> sigma_depth;
  real<lower=0> sigma_do;
  real<lower=0> sigma_thermal;
}

transformed parameters {
  // Predicted values for each indicator
  vector[N] conduct_hat;
  vector[N] depth_hat;
  vector[N] do_hat;
  vector[N] thermal_hat;

  // Linear predictor: indicator = intercept + loading * latent_quality
  conduct_hat = beta_conduct * stream_quality + a_conduct;
  depth_hat = beta_depth * stream_quality + a_depth;
  do_hat = beta_do * stream_quality + a_do;
  thermal_hat = beta_thermal * stream_quality + a_thermal;
}

model {
  // Prior on latent variable: standard normal (mean=0, sd=1)
  stream_quality ~ std_normal();

  // Priors on intercepts (weakly informative, centered at 0)
  a_conduct ~ normal(0, 10);
  a_depth ~ normal(0, 10);
  a_do ~ normal(0, 10);
  a_thermal ~ normal(0, 10);

  // Priors on factor loadings
  beta_conduct ~ normal(-1, 1);
  beta_depth ~ exponential(1.8);    // Directional prior for identification
  beta_do ~ exponential(1.8);       // Directional prior (higher DO = better quality)
  beta_thermal ~ normal(-1, 1);      // Unconstrained (expect negative: higher thermal = worse quality)

  // Priors on residual standard deviations
  sigma_conduct ~ exponential(0.5);
  sigma_depth ~ exponential(0.5);
  sigma_do ~ exponential(0.5);
  sigma_thermal ~ exponential(0.5);

  // Likelihood: observed indicators given predicted values
  conduct_log ~ normal(conduct_hat, sigma_conduct);
  max_depth ~ normal(depth_hat, sigma_depth);
  dissolved_oxygen ~ normal(do_hat, sigma_do);
  thermal ~ normal(thermal_hat, sigma_thermal);
}

generated quantities {
  // Posterior predictive checks: generate replicated data
  vector[N] conduct_rep;
  vector[N] depth_rep;
  vector[N] do_rep;
  vector[N] thermal_rep;

  for (i in 1:N) {
    conduct_rep[i] = normal_rng(conduct_hat[i], sigma_conduct);
    depth_rep[i] = normal_rng(depth_hat[i], sigma_depth);
    do_rep[i] = normal_rng(do_hat[i], sigma_do);
    thermal_rep[i] = normal_rng(thermal_hat[i], sigma_thermal);
  }
}

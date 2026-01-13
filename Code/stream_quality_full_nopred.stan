// Bayesian Latent Variable Model for Stream Quality - Full Model (No Predictors)
// Based on Scott Brown's ecological condition model
// https://github.com/cbrown5/ecological-condition-latent-model
//
// Model structure:
// - Latent variable "stream_quality" (nu) represents unobserved stream condition
// - 6 environmental indicators: conductivity, depth, DO, thermal, canopy, Q
// - NO predictors on latent variable
// - Outcome: trout presence/absence
// - Latent variable follows standard normal distribution

data {
  int<lower=1> N;  // Number of observations

  // Environmental indicators (all scaled)
  vector[N] conduct_log;
  vector[N] max_depth;
  vector[N] dissolved_oxygen;
  vector[N] thermal;
  vector[N] canopy_logit;
  vector[N] q_log;

  // Outcome
  int<lower=0, upper=1> trout[N];  // Binary: 1 = present, 0 = absent
}

parameters {
  // Latent variable: stream quality for each observation
  vector[N] stream_quality;  // Centered parameterization

  // Factor loadings (how strongly each indicator reflects stream quality)
  real beta_conduct;
  real<lower=0> beta_depth;  // Constrained positive (deeper = better quality)
  real<lower=0> beta_do;     // Constrained positive (higher DO = better quality)
  real beta_thermal;         // Can be negative (higher thermal = worse quality)
  real beta_canopy;
  real beta_q;

  // Intercepts for each indicator
  real a_conduct;
  real a_depth;
  real a_do;
  real a_thermal;
  real a_canopy;
  real a_q;

  // Residual standard deviations for each indicator
  real<lower=0> sigma_conduct;
  real<lower=0> sigma_depth;
  real<lower=0> sigma_do;
  real<lower=0> sigma_thermal;
  real<lower=0> sigma_canopy;
  real<lower=0> sigma_q;

  // Effect of stream quality on trout presence
  real beta_trout;    // Logistic regression coefficient
  real alpha_trout;   // Logistic regression intercept
}

transformed parameters {
  // Predicted values for each indicator
  vector[N] conduct_hat;
  vector[N] depth_hat;
  vector[N] do_hat;
  vector[N] thermal_hat;
  vector[N] canopy_hat;
  vector[N] q_hat;

  // Predicted probability of trout presence
  vector[N] trout_logit;

  // Linear predictor: indicator = intercept + loading * stream_quality
  conduct_hat = a_conduct + beta_conduct * stream_quality;
  depth_hat = a_depth + beta_depth * stream_quality;
  do_hat = a_do + beta_do * stream_quality;
  thermal_hat = a_thermal + beta_thermal * stream_quality;
  canopy_hat = a_canopy + beta_canopy * stream_quality;
  q_hat = a_q + beta_q * stream_quality;

  // Logistic regression for trout presence
  trout_logit = alpha_trout + beta_trout * stream_quality;
}

model {
  // Prior on latent variable (standard normal)
  stream_quality ~ std_normal();

  // Priors on intercepts (weakly informative, centered at 0)
  a_conduct ~ normal(0, 10);
  a_depth ~ normal(0, 10);
  a_do ~ normal(0, 10);
  a_thermal ~ normal(0, 10);
  a_canopy ~ normal(0, 10);
  a_q ~ normal(0, 10);

  // Priors on factor loadings
  beta_conduct ~ normal(-1, 1);
  beta_depth ~ exponential(1.8);    // Directional prior for identification
  beta_do ~ exponential(1.8);       // Directional prior (higher DO = better quality)
  beta_thermal ~ normal(-1, 1);     // Expect negative (higher thermal = worse quality)
  beta_canopy ~ normal(0, 2);
  beta_q ~ normal(0, 2);

  // Priors on residual standard deviations
  // Stronger priors (like reference model) to prevent overfitting with small N
  sigma_conduct ~ exponential(2);   // Mean = 0.5 (was 2.0)
  sigma_depth ~ exponential(2);
  sigma_do ~ exponential(2);
  sigma_thermal ~ exponential(2);
  sigma_canopy ~ exponential(2);
  sigma_q ~ exponential(2);

  // Priors on trout model
  alpha_trout ~ normal(0, 2);
  beta_trout ~ normal(0, 1);

  // Likelihood: observed indicators given predicted values
  conduct_log ~ normal(conduct_hat, sigma_conduct);
  max_depth ~ normal(depth_hat, sigma_depth);
  dissolved_oxygen ~ normal(do_hat, sigma_do);
  thermal ~ normal(thermal_hat, sigma_thermal);
  canopy_logit ~ normal(canopy_hat, sigma_canopy);
  q_log ~ normal(q_hat, sigma_q);

  // Likelihood: trout presence/absence
  trout ~ bernoulli_logit(trout_logit);
}

generated quantities {
  // Posterior predictive checks: generate replicated data
  vector[N] conduct_rep;
  vector[N] depth_rep;
  vector[N] do_rep;
  vector[N] thermal_rep;
  vector[N] canopy_rep;
  vector[N] q_rep;
  int trout_rep[N];

  // Log-likelihood for model comparison
  vector[N] log_lik;

  for (i in 1:N) {
    // Replicated indicator data
    conduct_rep[i] = normal_rng(conduct_hat[i], sigma_conduct);
    depth_rep[i] = normal_rng(depth_hat[i], sigma_depth);
    do_rep[i] = normal_rng(do_hat[i], sigma_do);
    thermal_rep[i] = normal_rng(thermal_hat[i], sigma_thermal);
    canopy_rep[i] = normal_rng(canopy_hat[i], sigma_canopy);
    q_rep[i] = normal_rng(q_hat[i], sigma_q);

    // Replicated trout data
    trout_rep[i] = bernoulli_logit_rng(trout_logit[i]);

    // Log-likelihood (sum of all components for each observation)
    log_lik[i] = normal_lpdf(conduct_log[i] | conduct_hat[i], sigma_conduct) +
                 normal_lpdf(max_depth[i] | depth_hat[i], sigma_depth) +
                 normal_lpdf(dissolved_oxygen[i] | do_hat[i], sigma_do) +
                 normal_lpdf(thermal[i] | thermal_hat[i], sigma_thermal) +
                 normal_lpdf(canopy_logit[i] | canopy_hat[i], sigma_canopy) +
                 normal_lpdf(q_log[i] | q_hat[i], sigma_q) +
                 bernoulli_logit_lpmf(trout[i] | trout_logit[i]);
  }
}

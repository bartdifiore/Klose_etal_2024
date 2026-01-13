#--------------------------------
## Comprehensive Model Diagnostics for Stan Reduced Model
## Tests for overfitting, convergence, and model fit
#--------------------------------

library(tidyverse)
library(rstan)
library(bayesplot)
library(loo)

#--------------------------------
## Load fitted model and data
#--------------------------------

# Load the fitted Stan model
fit <- readRDS("Models/stan_model_reduced_fit.rds")

# Reload the data (same as in fit_stan_model_reduced.R)
df <- read.csv("Data/cleaned_20251130.csv")

df_mod <- df %>%
  mutate(
    trout_present = as.integer(ifelse(trout_present_absent == "P", 1, 0)),
    max_depth_scaled = as.numeric(scale(max_depth_m)),
    do_scaled = as.numeric(scale(point_minimum_do_mg_l)),
    conduct_log_scaled = as.numeric(scale(log(conductivity_u_s_cm))),
    thermal_scaled = as.numeric(scale(thermal_index)),
    burned_coded = as.numeric(ifelse(burned_b_vs_unburned_u == "B", 1, 0)),
    wet_coded = as.numeric(ifelse(wet_or_dry_in_2016_1 == "W", 1, 0))
  ) %>%
  filter(!is.na(trout_present),
         !is.na(conduct_log_scaled),
         !is.na(max_depth_scaled),
         !is.na(do_scaled),
         !is.na(thermal_scaled),
         !is.na(burned_coded),
         !is.na(wet_coded))

cat("Sample size:", nrow(df_mod), "\n\n")

#--------------------------------
## 1. CONVERGENCE DIAGNOSTICS
#--------------------------------

cat("=============================================================\n")
cat("1. CONVERGENCE DIAGNOSTICS\n")
cat("=============================================================\n\n")

# Check divergences
sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
cat("Divergent transitions:", divergences, "\n")
if (divergences > 0) {
  cat("  WARNING: Divergences indicate sampling problems\n")
  cat("  Consider: increasing adapt_delta, reparameterization\n")
}

# Check Rhat
summary_fit <- summary(fit)$summary
rhats <- summary_fit[, "Rhat"]
max_rhat <- max(rhats, na.rm = TRUE)
n_bad_rhat <- sum(rhats > 1.01, na.rm = TRUE)

cat("\nRhat diagnostics:\n")
cat("  Max Rhat:", round(max_rhat, 4), "\n")
cat("  Parameters with Rhat > 1.01:", n_bad_rhat, "\n")
if (max_rhat > 1.01) {
  cat("  WARNING: Poor convergence (Rhat > 1.01)\n")
  cat("  Consider: running more iterations\n")
} else {
  cat("  GOOD: All chains converged (Rhat <= 1.01)\n")
}

# Check ESS
ess_bulk <- summary_fit[, "n_eff"]
min_ess <- min(ess_bulk, na.rm = TRUE)
n_low_ess <- sum(ess_bulk < 400, na.rm = TRUE)

cat("\nEffective Sample Size (ESS):\n")
cat("  Min ESS:", round(min_ess, 0), "\n")
cat("  Parameters with ESS < 400:", n_low_ess, "\n")
if (min_ess < 400) {
  cat("  WARNING: Low ESS indicates poor mixing\n")
} else {
  cat("  GOOD: Adequate effective sample sizes\n")
}

#--------------------------------
## 2. OVERFITTING DIAGNOSTICS: LOO-CV
#--------------------------------

cat("\n=============================================================\n")
cat("2. OVERFITTING DIAGNOSTICS: LOO Cross-Validation\n")
cat("=============================================================\n\n")

# Extract log-likelihood
log_lik <- extract_log_lik(fit, parameter_name = "log_lik")

# Compute LOO
loo_result <- loo(log_lik)
print(loo_result)

# Check Pareto k diagnostics
pareto_k <- loo_result$diagnostics$pareto_k
n_high_k <- sum(pareto_k > 0.7)

cat("\nPareto k diagnostics:\n")
cat("  Observations with k > 0.7:", n_high_k, "out of", length(pareto_k), "\n")
if (n_high_k > 0) {
  cat("  WARNING: Some observations are highly influential\n")
  high_k_indices <- which(pareto_k > 0.7)
  cat("  Problematic observations:", paste(high_k_indices, collapse = ", "), "\n")
} else {
  cat("  GOOD: No highly influential observations\n")
}

# Model complexity
p_loo <- loo_result$estimates["p_loo", "Estimate"]
n <- nrow(df_mod)
ratio <- n / p_loo

cat("\nModel complexity:\n")
cat("  Effective parameters (p_loo):", round(p_loo, 1), "\n")
cat("  Sample size (n):", n, "\n")
cat("  Ratio n/p_loo:", round(ratio, 2), "\n")

cat("\n*** COMPARISON TO FULL MODEL ***\n")
cat("  Full model (6 indicators): n/p_loo = 0.81\n")
cat("  Reduced model (4 indicators): n/p_loo =", round(ratio, 2), "\n")
cat("  Improvement:", round(ratio - 0.81, 2), "\n\n")

if (ratio < 5) {
  cat("  CAUTION: Model complexity still high (target ratio > 5)\n")
  cat("  But improved significantly from full model\n")
} else {
  cat("  GOOD: Sample size adequate for model complexity\n")
}

# Plot Pareto k diagnostics
pdf("Figures/diagnostics_reduced_pareto_k.pdf", width = 8, height = 6)
plot(loo_result, label_points = TRUE)
dev.off()
cat("\nPareto k plot saved to: Figures/diagnostics_reduced_pareto_k.pdf\n")

#--------------------------------
## 3. POSTERIOR PREDICTIVE CHECKS
#--------------------------------

cat("\n=============================================================\n")
cat("3. POSTERIOR PREDICTIVE CHECKS\n")
cat("=============================================================\n\n")

# Extract posterior predictive samples
conduct_rep <- extract(fit, "conduct_rep")[[1]]
depth_rep <- extract(fit, "depth_rep")[[1]]
do_rep <- extract(fit, "do_rep")[[1]]
thermal_rep <- extract(fit, "thermal_rep")[[1]]
trout_rep <- extract(fit, "trout_rep")[[1]]

# Sample 100 draws for plotting
n_draws <- min(100, nrow(conduct_rep))
draw_indices <- sample(1:nrow(conduct_rep), n_draws)

cat("Generating posterior predictive check plots...\n")

# PPC for continuous indicators - density overlay
pdf("Figures/diagnostics_reduced_ppc_indicators.pdf", width = 10, height = 8)

p1 <- ppc_dens_overlay(y = df_mod$conduct_log_scaled,
                       yrep = conduct_rep[draw_indices, ]) +
  ggtitle("PPC: Log Conductivity") +
  theme_minimal()

p2 <- ppc_dens_overlay(y = df_mod$max_depth_scaled,
                       yrep = depth_rep[draw_indices, ]) +
  ggtitle("PPC: Max Depth") +
  theme_minimal()

p3 <- ppc_dens_overlay(y = df_mod$do_scaled,
                       yrep = do_rep[draw_indices, ]) +
  ggtitle("PPC: Dissolved Oxygen") +
  theme_minimal()

p4 <- ppc_dens_overlay(y = df_mod$thermal_scaled,
                       yrep = thermal_rep[draw_indices, ]) +
  ggtitle("PPC: Thermal Index") +
  theme_minimal()

print(p1)
print(p2)
print(p3)
print(p4)
dev.off()

cat("Indicator PPC plots saved to: Figures/diagnostics_reduced_ppc_indicators.pdf\n")

# PPC for trout presence
pdf("Figures/diagnostics_reduced_ppc_trout.pdf", width = 10, height = 6)

p_trout1 <- ppc_bars(y = df_mod$trout_present,
                     yrep = trout_rep[draw_indices, ]) +
  ggtitle("PPC: Trout Presence/Absence") +
  theme_minimal()

p_trout2 <- ppc_stat(y = df_mod$trout_present,
                     yrep = trout_rep,
                     stat = "mean") +
  ggtitle("PPC: Trout Presence Rate") +
  theme_minimal()

print(p_trout1)
print(p_trout2)
dev.off()

cat("Trout PPC plots saved to: Figures/diagnostics_reduced_ppc_trout.pdf\n")

# Test statistics
cat("\nPosterior predictive p-values:\n")
test_mean <- function(y, yrep) {
  mean(rowMeans(yrep) > mean(y))
}

cat("  Conductivity:", round(test_mean(df_mod$conduct_log_scaled, conduct_rep), 3), "\n")
cat("  Depth:", round(test_mean(df_mod$max_depth_scaled, depth_rep), 3), "\n")
cat("  DO:", round(test_mean(df_mod$do_scaled, do_rep), 3), "\n")
cat("  Thermal:", round(test_mean(df_mod$thermal_scaled, thermal_rep), 3), "\n")
cat("  Trout rate:", round(test_mean(df_mod$trout_present, trout_rep), 3), "\n")
cat("\n  Note: Values near 0.5 indicate good fit\n")

#--------------------------------
## 4. BAYESIAN R² FOR INDICATORS
#--------------------------------

cat("\n=============================================================\n")
cat("4. BAYESIAN R² FOR EACH INDICATOR\n")
cat("=============================================================\n\n")

# Extract fitted values and sigmas
conduct_hat <- extract(fit, "conduct_hat")[[1]]
depth_hat <- extract(fit, "depth_hat")[[1]]
do_hat <- extract(fit, "do_hat")[[1]]
thermal_hat <- extract(fit, "thermal_hat")[[1]]

sigma_conduct <- extract(fit, "sigma_conduct")[[1]]
sigma_depth <- extract(fit, "sigma_depth")[[1]]
sigma_do <- extract(fit, "sigma_do")[[1]]
sigma_thermal <- extract(fit, "sigma_thermal")[[1]]

# Calculate Bayesian R²
bayes_r2 <- function(fitted, sigma) {
  var_fitted <- apply(fitted, 1, var)
  var_resid <- sigma^2
  r2 <- var_fitted / (var_fitted + var_resid)
  return(r2)
}

r2_conduct <- bayes_r2(conduct_hat, sigma_conduct)
r2_depth <- bayes_r2(depth_hat, sigma_depth)
r2_do <- bayes_r2(do_hat, sigma_do)
r2_thermal <- bayes_r2(thermal_hat, sigma_thermal)

cat("Bayesian R² (median [95% CI]):\n")
cat("  Conductivity: ", round(median(r2_conduct), 3),
    " [", round(quantile(r2_conduct, 0.025), 3),
    ", ", round(quantile(r2_conduct, 0.975), 3), "]\n", sep = "")
cat("  Depth:        ", round(median(r2_depth), 3),
    " [", round(quantile(r2_depth, 0.025), 3),
    ", ", round(quantile(r2_depth, 0.975), 3), "]\n", sep = "")
cat("  DO:           ", round(median(r2_do), 3),
    " [", round(quantile(r2_do, 0.025), 3),
    ", ", round(quantile(r2_do, 0.975), 3), "]\n", sep = "")
cat("  Thermal:      ", round(median(r2_thermal), 3),
    " [", round(quantile(r2_thermal, 0.025), 3),
    ", ", round(quantile(r2_thermal, 0.975), 3), "]\n", sep = "")

#--------------------------------
## SUMMARY AND RECOMMENDATIONS
#--------------------------------

cat("\n=============================================================\n")
cat("SUMMARY AND RECOMMENDATIONS\n")
cat("=============================================================\n\n")

# Overall assessment
issues <- c()

if (divergences > 0) issues <- c(issues, "Divergent transitions")
if (max_rhat > 1.01) issues <- c(issues, "Poor convergence (Rhat)")
if (min_ess < 400) issues <- c(issues, "Low ESS")
if (n_high_k > 0) issues <- c(issues, "Influential observations (high Pareto k)")
if (ratio < 1.5) {
  issues <- c(issues, "Model still quite complex for sample size")
} else if (ratio < 5) {
  issues <- c(issues, "Model complexity moderate (acceptable but not ideal)")
}

cat("Reduced model (4 indicators + tighter priors) vs Full model (6 indicators):\n")
cat("  - Dropped 2 indicators (canopy, Q)\n")
cat("  - Tightened all priors for regularization\n")
cat("  - Improvement in n/p_loo ratio:", round(ratio - 0.81, 2), "\n\n")

if (length(issues) == 0) {
  cat("*** OVERALL ASSESSMENT: Model diagnostics look good! ***\n\n")
} else {
  cat("*** ISSUES DETECTED ***\n\n")
  for (i in seq_along(issues)) {
    cat("  ", i, ". ", issues[i], "\n", sep = "")
  }
}

cat("\n=============================================================\n")
cat("Diagnostics complete!\n")
cat("=============================================================\n")

#--------------------------------
## Diagnostics for Full Hurdle Model
## Check convergence, LOO-CV, posterior predictive checks
#--------------------------------

library(tidyverse)
library(rstan)
library(bayesplot)
library(loo)

#--------------------------------
## Load fitted model
#--------------------------------

cat("Loading fitted model...\n")
fit <- readRDS("Models/stan_model_full_hurdle_fit.rds")

#--------------------------------
## Convergence Diagnostics
#--------------------------------

cat("\n=== CONVERGENCE DIAGNOSTICS ===\n\n")

# Divergences
sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
cat("Divergent transitions:", divergences, "\n")

# Rhat
summary_fit <- summary(fit)$summary
rhats <- summary_fit[, "Rhat"]
cat("Max Rhat:", max(rhats, na.rm = TRUE), "\n")
cat("Parameters with Rhat > 1.01:", sum(rhats > 1.01, na.rm = TRUE), "\n")
cat("Parameters with Rhat > 1.05:", sum(rhats > 1.05, na.rm = TRUE), "\n")

# ESS
ess_bulk <- summary_fit[, "n_eff"]
cat("Min ESS (bulk):", min(ess_bulk, na.rm = TRUE), "\n")
cat("Parameters with ESS < 100:", sum(ess_bulk < 100, na.rm = TRUE), "\n")

# List any problematic parameters
if (any(rhats > 1.01, na.rm = TRUE)) {
  cat("\nParameters with Rhat > 1.01:\n")
  problem_params <- names(rhats)[rhats > 1.01 & !is.na(rhats)]
  print(head(problem_params, 20))
}

#--------------------------------
## LOO-CV Analysis
#--------------------------------

cat("\n=== LOO-CV ANALYSIS ===\n\n")

# Extract log-likelihood
log_lik <- extract_log_lik(fit, parameter_name = "log_lik", merge_chains = FALSE)

# Compute relative effective sample sizes
r_eff <- relative_eff(exp(log_lik), cores = 4)

# Compute LOO
loo_result <- loo(log_lik, r_eff = r_eff, cores = 4)

print(loo_result)

# Extract key statistics
cat("\n=== LOO STATISTICS ===\n")
cat("N observations:", loo_result$estimates["elpd_loo", "Estimate"], "\n")
cat("p_loo (effective parameters):", loo_result$estimates["p_loo", "Estimate"], "\n")
cat("p_loo SE:", loo_result$estimates["p_loo", "SE"], "\n")
cat("ELPD (LOO):", loo_result$estimates["elpd_loo", "Estimate"], "\n")
cat("ELPD SE:", loo_result$estimates["elpd_loo", "SE"], "\n")

# Sample size to effective parameters ratio
N_obs <- nrow(log_lik)
p_loo <- loo_result$estimates["p_loo", "Estimate"]
cat("\nn/p_loo ratio:", N_obs / p_loo, "\n")
cat("(Should be > 5 ideally, > 1 minimally)\n")

# Pareto k diagnostics
cat("\n=== PARETO K DIAGNOSTICS ===\n")
pareto_k <- loo_result$diagnostics$pareto_k
cat("Pareto k statistics:\n")
cat("  Good (k < 0.5):", sum(pareto_k < 0.5), "/", length(pareto_k),
    sprintf("(%.1f%%)\n", 100 * mean(pareto_k < 0.5)))
cat("  OK (0.5 <= k < 0.7):", sum(pareto_k >= 0.5 & pareto_k < 0.7), "/", length(pareto_k),
    sprintf("(%.1f%%)\n", 100 * mean(pareto_k >= 0.5 & pareto_k < 0.7)))
cat("  Bad (0.7 <= k < 1):", sum(pareto_k >= 0.7 & pareto_k < 1), "/", length(pareto_k),
    sprintf("(%.1f%%)\n", 100 * mean(pareto_k >= 0.7 & pareto_k < 1)))
cat("  Very bad (k >= 1):", sum(pareto_k >= 1), "/", length(pareto_k),
    sprintf("(%.1f%%)\n", 100 * mean(pareto_k >= 1)))

# Plot Pareto k values
plot(loo_result, label_points = TRUE)

#--------------------------------
## Posterior Predictive Checks
#--------------------------------

cat("\n=== POSTERIOR PREDICTIVE CHECKS ===\n\n")

# Load data to compare
df <- read.csv("Data/Data_20240408.csv")

df_mod <- df %>%
  mutate(
    trout_count = as.integer(total.trout),
    conduct_log_scaled = as.numeric(scale(log(Conductivity.uS.cm.))),
    max_depth_scaled = as.numeric(scale(Max.depth.m)),
    do_scaled = as.numeric(scale(as.numeric(Point.Minimum.DO.mg.L))),
    thermal_scaled = as.numeric(scale(Thermal.index)),
    canopy_logit_scaled = as.numeric(scale(qlogis(Average.canopy.cover/100))),
    q_log_scaled = as.numeric(scale(log(`Q.estimate..m3.s.`))),
    burned_coded = as.numeric(ifelse(`Burned..B..vs..unburned..U.` == "B", 1, 0)),
    wet_coded = as.numeric(ifelse(`Wet.or.dry.in.2016` == "W", 1, 0))
  ) %>%
  filter(!is.na(trout_count),
         !is.na(conduct_log_scaled),
         !is.na(max_depth_scaled),
         !is.na(do_scaled),
         !is.na(thermal_scaled),
         !is.na(canopy_logit_scaled),
         !is.na(q_log_scaled),
         !is.na(burned_coded),
         !is.na(wet_coded))

# Extract posterior predictive samples
conduct_rep <- as.matrix(fit, pars = "conduct_rep")
depth_rep <- as.matrix(fit, pars = "depth_rep")
do_rep <- as.matrix(fit, pars = "do_rep")
thermal_rep <- as.matrix(fit, pars = "thermal_rep")
canopy_rep <- as.matrix(fit, pars = "canopy_rep")
q_rep <- as.matrix(fit, pars = "q_rep")
trout_count_rep <- as.matrix(fit, pars = "trout_count_rep")

# PPC: Conductivity
cat("Posterior predictive check: Conductivity\n")
ppc_dens_overlay(y = df_mod$conduct_log_scaled,
                 yrep = conduct_rep[1:100, ]) +
  labs(title = "PPC: Conductivity (log-scaled)")

# PPC: Depth
cat("Posterior predictive check: Depth\n")
ppc_dens_overlay(y = df_mod$max_depth_scaled,
                 yrep = depth_rep[1:100, ]) +
  labs(title = "PPC: Max Depth (scaled)")

# PPC: Trout Presence (count > 0)
cat("\nPosterior predictive check: Trout Presence\n")
# Proportion of sites with trout present
obs_prop_present <- mean(df_mod$trout_count > 0)
pred_prop_present <- apply(trout_count_rep > 0, 1, mean)

cat("Observed proportion with trout:", obs_prop_present, "\n")
cat("Predicted proportion (mean):", mean(pred_prop_present), "\n")
cat("Predicted proportion (95% CI): [",
    quantile(pred_prop_present, 0.025), ",",
    quantile(pred_prop_present, 0.975), "]\n")

hist(pred_prop_present, breaks = 30, main = "PPC: Proportion Sites with Trout",
     xlab = "Proportion", col = "lightblue")
abline(v = obs_prop_present, col = "red", lwd = 2)
legend("topright", legend = "Observed", col = "red", lwd = 2)

# PPC: Trout Count (conditional on presence)
cat("\nPosterior predictive check: Trout Count (given presence)\n")

# Extract counts for sites with trout present
present_sites <- which(df_mod$trout_count > 0)
obs_counts <- df_mod$trout_count[present_sites]
pred_counts <- trout_count_rep[, present_sites]

cat("Observed mean count (given presence):", mean(obs_counts), "\n")
cat("Predicted mean count (given presence):", mean(pred_counts), "\n")
cat("Observed median count:", median(obs_counts), "\n")
cat("Predicted median count:", median(pred_counts), "\n")

# Distribution comparison
cat("\nCount distribution (given presence):\n")
cat("Observed:\n")
print(table(obs_counts))
cat("\nPredicted (mean across iterations):\n")
pred_counts_mean <- apply(pred_counts, 2, mean)
print(summary(pred_counts_mean))

# Histogram comparison
par(mfrow = c(1, 2))
hist(obs_counts, breaks = 20, main = "Observed Counts (given presence)",
     xlab = "Trout Count", col = "lightblue")
hist(pred_counts, breaks = 20, main = "Predicted Counts (given presence)",
     xlab = "Trout Count", col = "lightgreen")
par(mfrow = c(1, 1))

#--------------------------------
## Parameter Summaries
#--------------------------------

cat("\n=== KEY PARAMETER ESTIMATES ===\n\n")

cat("HURDLE MODEL PART 1 (Presence):\n")
print(summary(fit, pars = c("alpha_trout_presence", "beta_trout_presence"))$summary)

cat("\nHURDLE MODEL PART 2 (Abundance | Presence):\n")
print(summary(fit, pars = c("alpha_trout_abundance", "beta_trout_abundance"))$summary)

cat("\nFACTOR LOADINGS:\n")
print(summary(fit, pars = c("beta_conduct", "beta_depth", "beta_do",
                            "beta_thermal", "beta_canopy", "beta_q"))$summary)

cat("\nEFFECTS ON STREAM QUALITY:\n")
print(summary(fit, pars = c("beta_burned", "beta_wet"))$summary)

#--------------------------------
## Save diagnostics
#--------------------------------

# Save LOO object
saveRDS(loo_result, "Models/stan_model_full_hurdle_loo.rds")
cat("\nLOO object saved to: Models/stan_model_full_hurdle_loo.rds\n")

cat("\n=== DIAGNOSTICS COMPLETE ===\n")

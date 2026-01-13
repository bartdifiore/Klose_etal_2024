#--------------------------------
## Fit Reduced Stan latent variable model
## Using rstan
## Includes: 4 indicators (dropped canopy & Q), burn/drought predictors, trout outcome
## TIGHTER PRIORS to reduce overfitting with n=35
#--------------------------------

library(tidyverse)
library(rstan)
library(bayesplot)

# Set options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = FALSE)

#--------------------------------
## Load and prepare data (2025 dataset - no Q variable needed)
#--------------------------------

df <- read.csv("Data/cleaned_20251130.csv")

df_mod <- df %>%
  mutate(
    # Response variable
    trout_present = as.integer(ifelse(trout_present_absent == "P", 1, 0)),

    # Transform continuous predictors (scaled for modeling)
    max_depth_scaled = as.numeric(scale(max_depth_m)),
    do_scaled = as.numeric(scale(point_minimum_do_mg_l)),
    conduct_log_scaled = as.numeric(scale(log(conductivity_u_s_cm))),
    thermal_scaled = as.numeric(scale(thermal_index)),

    # Categorical predictors (0/1 coded for Stan)
    burned_coded = as.numeric(ifelse(burned_b_vs_unburned_u == "B", 1, 0)),
    wet_coded = as.numeric(ifelse(wet_or_dry_in_2016_1 == "W", 1, 0))
  ) %>%
  # Remove rows with missing values in key variables
  filter(!is.na(trout_present),
         !is.na(conduct_log_scaled),
         !is.na(max_depth_scaled),
         !is.na(do_scaled),
         !is.na(thermal_scaled),
         !is.na(burned_coded),
         !is.na(wet_coded))

cat("Sample size:", nrow(df_mod), "\n")

#--------------------------------
## Prepare data for Stan
#--------------------------------

stan_data <- list(
  N = nrow(df_mod),
  # Indicators (only 4 - dropped canopy and Q)
  conduct_log = df_mod$conduct_log_scaled,
  max_depth = df_mod$max_depth_scaled,
  dissolved_oxygen = df_mod$do_scaled,
  thermal = df_mod$thermal_scaled,
  # Outcome
  trout = df_mod$trout_present
)

#--------------------------------
## Compile and fit Stan model
#--------------------------------

# Compile the model
model <- stan_model("Code/stream_quality_reduced.stan")

# Fit the model
fit <- sampling(
  object = model,
  data = stan_data,
  chains = 4,
  cores = 4,
  warmup = 2000,
  iter = 8000,  # Total iterations = warmup + sampling
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  refresh = 500  # Print progress every 500 iterations
)

#--------------------------------
## Print summary
#--------------------------------

cat("\n=== FACTOR LOADINGS ===\n")
print(fit, pars = c("beta_conduct", "beta_depth", "beta_do", "beta_thermal"))

cat("\n=== INTERCEPTS ===\n")
print(fit, pars = c("a_conduct", "a_depth", "a_do", "a_thermal"))

cat("\n=== RESIDUAL SDs ===\n")
print(fit, pars = c("sigma_conduct", "sigma_depth", "sigma_do", "sigma_thermal"))

cat("\n=== EFFECT ON TROUT PRESENCE ===\n")
print(fit, pars = c("alpha_trout", "beta_trout"))

#--------------------------------
## Diagnostics
#--------------------------------

cat("\n=== CONVERGENCE DIAGNOSTICS ===\n")

# Check divergences
sampler_params <- get_sampler_params(fit, inc_warmup = FALSE)
divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
cat("Divergent transitions:", divergences, "\n")

# Check Rhat
summary_fit <- summary(fit)$summary
rhats <- summary_fit[, "Rhat"]
cat("Max Rhat:", max(rhats, na.rm = TRUE), "\n")
cat("Parameters with Rhat > 1.01:", sum(rhats > 1.01, na.rm = TRUE), "\n")

# Check ESS
ess_bulk <- summary_fit[, "n_eff"]
cat("Min ESS (bulk):", min(ess_bulk, na.rm = TRUE), "\n")

#--------------------------------
## Extract and visualize results
#--------------------------------

# Extract draws (convert to format compatible with bayesplot)
draws <- as.array(fit)

# Plot factor loadings
p1 <- mcmc_areas(draws,
                pars = c("beta_conduct", "beta_depth", "beta_do", "beta_thermal"),
                prob = 0.95) +
  labs(title = "Factor Loadings (Stream Quality Indicators)",
       subtitle = "4 indicators with tighter priors")

print(p1)

# Plot effect of stream quality on trout
p3 <- mcmc_areas(draws,
                pars = c("beta_trout"),
                prob = 0.95) +
  labs(title = "Effect of Stream Quality on Trout Presence",
       subtitle = "Logistic regression coefficient")

print(p3)

# Plot posterior of latent variable for first 10 observations
stream_quality_vars <- paste0("stream_quality[", 1:10, "]")
p4 <- mcmc_intervals(draws, pars = stream_quality_vars) +
  labs(title = "Latent Stream Quality (First 10 sites)",
       x = "Stream Quality Score")

print(p4)

#--------------------------------
## Save results
#--------------------------------

# Save the fitted model
saveRDS(fit, file = "Models/stan_model_reduced_fit.rds")

# Save summary
summary_df <- as.data.frame(summary(fit)$summary)
write.csv(summary_df, "Models/stan_model_reduced_summary.csv", row.names = TRUE)

cat("\nModel fitting complete!\n")
cat("Model saved to: Models/stan_model_reduced_fit.rds\n")
cat("Summary saved to: Models/stan_model_reduced_summary.csv\n")

#--------------------------------
## Fit Stan latent variable model directly
## Using rstan
#--------------------------------

library(tidyverse)
library(rstan)
library(bayesplot)

# Set options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = FALSE)

#--------------------------------
## Load and prepare data
#--------------------------------

df <- read.csv("Data/cleaned_20251130.csv")

df_mod <- df %>%
  mutate(
    trout_present = as.integer(ifelse(trout_present_absent == "P", 1, 0)),
    max_depth_scaled = as.numeric(scale(max_depth_m)),
    do_scaled = as.numeric(scale(point_minimum_do_mg_l)),
    conduct_log_scaled = as.numeric(scale(log(conductivity_u_s_cm))),
    thermal_scaled = as.numeric(scale(thermal_index)),
    burned = as.factor(burned_b_vs_unburned_u),
    wet_dry = as.factor(wet_or_dry_in_2016_1)
  ) %>%
  filter(!is.na(trout_present),
         !is.na(conduct_log_scaled),
         !is.na(max_depth_scaled),
         !is.na(do_scaled),
         !is.na(thermal_scaled))

cat("Sample size:", nrow(df_mod), "\n")

#--------------------------------
## Prepare data for Stan
#--------------------------------

stan_data <- list(
  N = nrow(df_mod),
  conduct_log = df_mod$conduct_log_scaled,
  max_depth = df_mod$max_depth_scaled,
  dissolved_oxygen = df_mod$do_scaled,
  thermal = df_mod$thermal_scaled
)

#--------------------------------
## Compile and fit Stan model
#--------------------------------

# Compile the model
model <- stan_model("Code/stream_quality_latent.stan")

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

# Print summary
print(fit, pars = c("beta_conduct", "beta_depth", "beta_do", "beta_thermal",
                   "a_conduct", "a_depth", "a_do", "a_thermal",
                   "sigma_conduct", "sigma_depth", "sigma_do", "sigma_thermal"))

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
mcmc_areas(draws, pars = c("beta_conduct", "beta_depth", "beta_do", "beta_thermal"),
           prob = 0.95) +
  labs(title = "Factor Loadings (Stream Quality Indicators)",
       subtitle = "How strongly each indicator reflects stream quality")

# Plot posterior of latent variable for first 10 observations
stream_quality_vars <- paste0("stream_quality[", 1:10, "]")
mcmc_intervals(draws, pars = stream_quality_vars) +
  labs(title = "Latent Stream Quality (First 10 sites)",
       x = "Stream Quality Score")

#--------------------------------
## Save results
#--------------------------------

# Save the fitted model
saveRDS(fit, file = "Models/stan_model_fit.rds")

# Save summary
summary_df <- as.data.frame(summary(fit)$summary)
write.csv(summary_df, "Models/stan_model_summary.csv", row.names = TRUE)

cat("\nModel fitting complete!\n")
cat("Model saved to: Models/stan_model_fit.rds\n")
cat("Summary saved to: Models/stan_model_summary.csv\n")

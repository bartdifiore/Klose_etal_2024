#--------------------------------
## Fit Full Stan latent variable model WITH HURDLE COMPONENT
## Using rstan
## Includes: 6 indicators, burn/drought predictors,
##           HURDLE MODEL for trout (presence + abundance)
#--------------------------------

library(tidyverse)
library(rstan)
library(bayesplot)

# Set options
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = FALSE)

#--------------------------------
## Load and prepare data (2024 dataset with Q variable)
#--------------------------------

df <- read.csv("Data/Data_20240408.csv")

df_mod <- df %>%
  mutate(
    # Response variable - HURDLE MODEL (count, where 0 = absent)
    trout_count = as.integer(total.trout),  # Total trout count (0 for absent)

    # Transform continuous predictors (scaled for modeling)
    max_depth_scaled = as.numeric(scale(Max.depth.m)),
    do_scaled = as.numeric(scale(as.numeric(Point.Minimum.DO.mg.L))),
    conduct_log_scaled = as.numeric(scale(log(Conductivity.uS.cm.))),
    canopy_logit_scaled = as.numeric(scale(qlogis(Average.canopy.cover/100))),
    thermal_scaled = as.numeric(scale(Thermal.index)),
    q_log_scaled = as.numeric(scale(log(`Q.estimate..m3.s.`))),

    # Categorical predictors (0/1 coded for Stan)
    burned_coded = as.numeric(ifelse(`Burned..B..vs..unburned..U.` == "B", 1, 0)),
    wet_coded = as.numeric(ifelse(`Wet.or.dry.in.2016` == "W", 1, 0))
  ) %>%
  # Remove rows with missing values in key variables
  filter(!is.na(trout_count),
         !is.na(conduct_log_scaled),
         !is.na(max_depth_scaled),
         !is.na(do_scaled),
         !is.na(thermal_scaled),
         !is.na(canopy_logit_scaled),
         !is.na(q_log_scaled),
         !is.na(burned_coded),
         !is.na(wet_coded))

cat("Sample size:", nrow(df_mod), "\n")
cat("Sites with trout present (count > 0):", sum(df_mod$trout_count > 0), "\n")
cat("Sites with trout absent (count = 0):", sum(df_mod$trout_count == 0), "\n")

# Summary of trout counts
cat("\nTrout count distribution (all sites):\n")
print(summary(df_mod$trout_count))

cat("\nTrout count distribution (given presence, count > 0):\n")
df_mod %>%
  filter(trout_count > 0) %>%
  summarize(
    min = min(trout_count),
    q25 = quantile(trout_count, 0.25),
    median = median(trout_count),
    mean = mean(trout_count),
    q75 = quantile(trout_count, 0.75),
    max = max(trout_count)
  ) %>%
  print()

#--------------------------------
## Prepare data for Stan - HURDLE MODEL
#--------------------------------

stan_data <- list(
  N = nrow(df_mod),
  # Indicators
  conduct_log = df_mod$conduct_log_scaled,
  max_depth = df_mod$max_depth_scaled,
  dissolved_oxygen = df_mod$do_scaled,
  thermal = df_mod$thermal_scaled,
  canopy_logit = df_mod$canopy_logit_scaled,
  q_log = df_mod$q_log_scaled,
  # Predictors
  burned = df_mod$burned_coded,
  wet = df_mod$wet_coded,
  # Outcome - HURDLE MODEL (count data, 0 = absent)
  trout_count = df_mod$trout_count
)

cat("\nHurdle model structure:\n")
cat("  Total observations:", stan_data$N, "\n")
cat("  Observations with trout (count > 0):", sum(stan_data$trout_count > 0), "\n")
cat("  Observations without trout (count = 0):", sum(stan_data$trout_count == 0), "\n")

#--------------------------------
## Compile and fit Stan model
#--------------------------------

# Compile the model
cat("\nCompiling Stan model...\n")
model <- stan_model("Code/stream_quality_full_hurdle.stan")
#model <- stan_model("Code/stream_quality_full_hurdle_wintercept.stan")


# Fit the model
cat("\nFitting Stan model...\n")
cat("This may take several minutes...\n")

fit <- sampling(
  object = model,
  data = stan_data,
  chains = 4,
  cores = 4,
  warmup = 2000,
  iter = 4000,  # Total iterations = warmup + sampling
  seed = 123,
  control = list(adapt_delta = 0.99, max_treedepth = 12),
  refresh = 500  # Print progress every 500 iterations
)

#--------------------------------
## Print summary
#--------------------------------

cat("\n=== FACTOR LOADINGS ===\n")
print(fit, pars = c("beta_conduct", "beta_depth", "beta_do", "beta_thermal",
                   "beta_canopy", "beta_q"))


cat("\n=== INTERCEPTS ===\n")
print(fit, pars = c("a_conduct", "a_depth", "a_do", "a_thermal",
                   "a_canopy", "a_q"))

cat("\n=== RESIDUAL SDs ===\n")
print(fit, pars = c("sigma_conduct", "sigma_depth", "sigma_do", "sigma_thermal",
                   "sigma_canopy", "sigma_q"))

cat("\n=== EFFECTS ON STREAM QUALITY ===\n")
print(fit, pars = c("beta_burned", "beta_wet", "alpha_sq"))

cat("\n=== EFFECTS ON STREAM QUALITY ===\n")
print(fit, pars = c("mu_unburned_dry", "mu_unburned_wet", "mu_burned_dry", "mu_burned_wet"))

cat("\n=== HURDLE MODEL PART 1: PRESENCE/ABSENCE ===\n")
print(fit, pars = c("alpha_trout_presence", "beta_trout_presence"))

cat("\n=== HURDLE MODEL PART 2: ABUNDANCE (GIVEN PRESENCE) ===\n")
print(fit, pars = c("alpha_trout_abundance", "beta_trout_abundance"))

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
                pars = c("beta_conduct", "beta_depth", "beta_do",
                        "beta_thermal", "beta_canopy", "beta_q"),
                prob = 0.95) +
  labs(title = "Factor Loadings (Stream Quality Indicators)",
       subtitle = "How strongly each indicator reflects stream quality")

print(p1)

# Plot effects of burn and drought on stream quality
p2 <- mcmc_areas(draws,
                pars = c("beta_burned", "beta_wet"),
                prob = 0.95) +
  labs(title = "Effects of Disturbance on Stream Quality",
       subtitle = "Burned vs Unburned, Wet vs Dry")

print(p2)

# Plot HURDLE MODEL PART 1: Effect of stream quality on trout presence
p3 <- mcmc_areas(draws,
                pars = c("beta_trout_presence"),
                prob = 0.95) +
  labs(title = "Effect of Stream Quality on Trout PRESENCE",
       subtitle = "Logistic regression coefficient (Hurdle Part 1)")

print(p3)

# Plot HURDLE MODEL PART 2: Effect of stream quality on trout abundance
p4 <- mcmc_areas(draws,
                pars = c("beta_trout_abundance"),
                prob = 0.95) +
  labs(title = "Effect of Stream Quality on Trout ABUNDANCE (given presence)",
       subtitle = "Log-linear coefficient (Hurdle Part 2, zero-truncated Poisson)")

print(p4)

# Plot posterior of latent variable for first 10 observations
stream_quality_vars <- paste0("stream_quality[", 1:10, "]")
p5 <- mcmc_intervals(draws, pars = stream_quality_vars) +
  labs(title = "Latent Stream Quality (First 10 sites)",
       x = "Stream Quality Score")

print(p5)

#--------------------------------
## Compare presence vs abundance effects
#--------------------------------

cat("\n=== COMPARISON: PRESENCE vs ABUNDANCE EFFECTS ===\n")

# Extract posterior samples
presence_effect <- as.matrix(fit, pars = "beta_trout_presence")
abundance_effect <- as.matrix(fit, pars = "beta_trout_abundance")

cat("\nEffect on PRESENCE:\n")
cat("  Mean:", mean(presence_effect), "\n")
cat("  95% CI: [", quantile(presence_effect, 0.025), ",",
    quantile(presence_effect, 0.975), "]\n")
cat("  P(beta > 0):", mean(presence_effect > 0), "\n")

cat("\nEffect on ABUNDANCE (given presence):\n")
cat("  Mean:", mean(abundance_effect), "\n")
cat("  95% CI: [", quantile(abundance_effect, 0.025), ",",
    quantile(abundance_effect, 0.975), "]\n")
cat("  P(beta > 0):", mean(abundance_effect > 0), "\n")

cat("\nInterpretation:\n")
cat("  - Positive beta_trout_presence: Higher quality increases probability of presence\n")
cat("  - Positive beta_trout_abundance: Higher quality increases abundance (given present)\n")
cat("  - These effects can differ! E.g., quality might affect presence but not abundance\n")

#--------------------------------
## Save results
#--------------------------------

# Save the fitted model
#saveRDS(fit, file = "Models/stan_model_full_hurdle_fit_wintercept.rds")
saveRDS(fit, file = "Models/stan_model_full_hurdle_fit.rds")

# Save summary
summary_df <- as.data.frame(summary(fit)$summary)
# write.csv(summary_df, "Models/stan_model_full_hurdle_wintercept_summary.csv", row.names = TRUE)
write.csv(summary_df, "Models/stan_model_full_hurdle_summary.csv", row.names = TRUE)

cat("\nModel fitting complete!\n")
cat("Model saved to: Models/stan_model_full_hurdle_fit.rds\n")
cat("Summary saved to: Models/stan_model_full_hurdle_summary.csv\n")

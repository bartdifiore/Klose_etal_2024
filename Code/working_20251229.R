#--------------------------------
## Working file for latent variable model development
## Created: 2025-12-29
## Bayesian Latent Variable SEM using brms - Brown's Approach
##
## Based on approach from:
## - Scott Brown: https://github.com/cbrown5/ecological-condition-latent-model
## - Fixes latent variance to 1 (standard normal) rather than fixing first loading
## - Uses directional prior (exponential) on one indicator to prevent parameter switching
#--------------------------------

# Load libraries
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)

# Set options for brms
options(mc.cores = parallel::detectCores())

#--------------------------------
## Load and prepare data
#--------------------------------

df <- read.csv("Data/cleaned_20251130.csv")

# Prepare data for modeling
df_mod <- df %>%
  mutate(
    # Response variables
    trout_present = as.integer(ifelse(trout_present_absent == "P", 1, 0)),

    # Transform continuous predictors (scaled for modeling)
    max_depth_scaled = as.numeric(scale(max_depth_m)),
    do_scaled = as.numeric(scale(point_minimum_do_mg_l)),
    conduct_log_scaled = as.numeric(scale(log(conductivity_u_s_cm))),
    canopy_logit_scaled = as.numeric(scale(qlogis(average_canopy_cover/100))),
    thermal_scaled = as.numeric(scale(thermal_index)),

    # Exogenous categorical predictors
    burned = as.factor(burned_b_vs_unburned_u),
    wet_dry = as.factor(wet_or_dry_in_2016_1),

    # CRITICAL: Add NA column for latent variable
    # This is required for brms mi() approach
    stream_quality = as.numeric(NA)
  ) %>%
  # Remove rows with missing values in key variables
  filter(!is.na(trout_present),
         !is.na(conduct_log_scaled),
         !is.na(max_depth_scaled),
         !is.na(do_scaled),
         !is.na(thermal_scaled))

# Check sample size
cat("Sample size after filtering:", nrow(df_mod), "\n")

#--------------------------------
## Create fake dataset for testing sample size effects
#--------------------------------

set.seed(123)  # For reproducibility

# Get summary statistics from real data
real_stats <- df_mod %>%
  summarise(
    mean_depth = mean(max_depth_m, na.rm = TRUE),
    sd_depth = sd(max_depth_m, na.rm = TRUE),
    mean_do = mean(point_minimum_do_mg_l, na.rm = TRUE),
    sd_do = sd(point_minimum_do_mg_l, na.rm = TRUE),
    mean_conduct = mean(log(conductivity_u_s_cm), na.rm = TRUE),
    sd_conduct = sd(log(conductivity_u_s_cm), na.rm = TRUE),
    mean_canopy = mean(average_canopy_cover/100, na.rm = TRUE),
    sd_canopy = sd(average_canopy_cover/100, na.rm = TRUE),
    mean_thermal = mean(thermal_index, na.rm = TRUE),
    sd_thermal = sd(thermal_index, na.rm = TRUE),
    prop_trout_present = mean(trout_present),
    prop_burned = mean(burned == "B"),
    prop_wet = mean(wet_dry == "W")
  )

cat("\nGenerating fake dataset with n=1000...\n")

# Generate fake data with same statistical properties
n_fake <- 1000

df_fake <- tibble(
  # Continuous variables - generated from normal distributions
  max_depth_m = rnorm(n_fake, mean = real_stats$mean_depth, sd = real_stats$sd_depth),
  point_minimum_do_mg_l = rnorm(n_fake, mean = real_stats$mean_do, sd = real_stats$sd_do),
  conductivity_log = rnorm(n_fake, mean = real_stats$mean_conduct, sd = real_stats$sd_conduct),
  canopy_prop = rnorm(n_fake, mean = real_stats$mean_canopy, sd = real_stats$sd_canopy),
  thermal_index = rnorm(n_fake, mean = real_stats$mean_thermal, sd = real_stats$sd_thermal),

  # Categorical predictors
  burned_b_vs_unburned_u = sample(c("B", "U"), n_fake, replace = TRUE,
                                   prob = c(real_stats$prop_burned, 1 - real_stats$prop_burned)),
  wet_or_dry_in_2016_1 = sample(c("W", "D"), n_fake, replace = TRUE,
                                 prob = c(real_stats$prop_wet, 1 - real_stats$prop_wet)),

  # Binary outcome
  trout_present_absent = sample(c("P", "A"), n_fake, replace = TRUE,
                                 prob = c(real_stats$prop_trout_present, 1 - real_stats$prop_trout_present))
) %>%
  mutate(
    # Apply constraints to keep variables realistic
    max_depth_m = pmax(0.01, max_depth_m),  # Depth must be positive
    point_minimum_do_mg_l = pmax(0, point_minimum_do_mg_l),  # DO must be non-negative
    conductivity_u_s_cm = exp(conductivity_log),  # Back-transform, always positive
    average_canopy_cover = pmin(99.9, pmax(0.1, canopy_prop * 100)),  # Constrain to (0, 100)
    thermal_index = pmax(0, thermal_index)  # Thermal index should be non-negative
  ) %>%
  select(-conductivity_log, -canopy_prop)  # Remove intermediate variables

# Prepare fake data same way as real data
df_fake_mod <- df_fake %>%
  mutate(
    trout_present = as.integer(ifelse(trout_present_absent == "P", 1, 0)),
    max_depth_scaled = as.numeric(scale(max_depth_m)),
    do_scaled = as.numeric(scale(point_minimum_do_mg_l)),
    conduct_log_scaled = as.numeric(scale(log(conductivity_u_s_cm))),
    canopy_logit_scaled = as.numeric(scale(qlogis(average_canopy_cover/100))),
    thermal_scaled = as.numeric(scale(thermal_index)),
    burned = as.factor(burned_b_vs_unburned_u),
    wet_dry = as.factor(wet_or_dry_in_2016_1),
    stream_quality = as.numeric(NA)
  )

cat("Fake dataset created: n =", nrow(df_fake_mod), "\n")
cat("Real dataset size: n =", nrow(df_mod), "\n\n")

#--------------------------------
## Define formula components
#--------------------------------

# Environmental indicators caused by latent stream quality
bf_conduct <- bf(conduct_log_scaled ~ 0 + mi(stream_quality))
bf_depth <- bf(max_depth_scaled ~ 0 + mi(stream_quality))
bf_do <- bf(do_scaled ~ 0 + mi(stream_quality))
bf_thermal <- bf(thermal_scaled ~ 0 + mi(stream_quality))

# Trout presence predicted by latent stream quality
bf_trout <- bf(trout_present ~ 0 + mi(stream_quality), family = bernoulli())

#--------------------------------
## Model 1: Indicators + Latent Variable Only
## Simple latent variable model with no predictors or outcomes
## BROWN'S APPROACH: Fix latent variance to 1, use directional prior on one loading
#--------------------------------

cat("\n=== MODEL 1: Indicators + Latent Variable (Brown's Approach) ===\n")

# Latent variable with no predictors (intercept only, removed with + 0)
bf_quality1 <- bf(stream_quality | mi() ~ 0)

# Combine components
model1_formula <-
  bf_quality1 +
  bf_conduct +
  bf_depth +
  bf_do +
  bf_thermal +
  set_rescor(FALSE)

# Set priors for identification - BROWN'S APPROACH
# Key differences from Byrnes:
# - Fix latent variance to 1: normal(0, 1) instead of gamma(11, 11)
# - Use exponential(1.8) on thermal_index to prevent parameter switching
# - Use normal(0, 5) on other loadings (weakly informative)
# - gamma(1, 0.5) for indicator residual variances (same as Byrnes)
priors1 <-
  prior(normal(0, 1), class = "sigma", resp = "streamquality") +  # FIX LATENT SD TO 1
  prior(exponential(1.8), class = "b", coef = "mistream_quality", resp = "thermalscaled") +  # DIRECTIONAL
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "conductlogscaled") +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "maxdepthscaled") +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "doscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "conductlogscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "maxdepthscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "doscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "thermalscaled")

# Fit the model
# Increased iterations and adaptation settings for better convergence
model1_fit <- brm(
  model1_formula,
  data = df_mod,
  prior = priors1,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  file = "Models/working_model1_20251229",
  file_refit = "on_change"
)

summary(model1_fit)

# Extract and visualize factor loadings
cat("\nModel 1 - Factor Loadings:\n")
posterior_summary(model1_fit,
                 variable = c("bsp_conductlogscaled_mistream_quality",
                             "bsp_maxdepthscaled_mistream_quality",
                             "bsp_doscaled_mistream_quality",
                             "bsp_thermalscaled_mistream_quality"))

# Check latent variance (should be close to 1)
cat("\nModel 1 - Latent Variance (should be ~1):\n")
posterior_summary(model1_fit,
                 variable = "sigma_streamquality")

# Check convergence
cat("\nConvergence diagnostics:\n")
cat("Divergent transitions:", sum(nuts_params(model1_fit)$Value), "\n")
cat("Max Rhat:", max(rhat(model1_fit), na.rm = TRUE), "\n")
cat("Min ESS ratio:", min(neff_ratio(model1_fit), na.rm = TRUE), "\n")

#--------------------------------
## Model 1 FAKE: Same model with fake data (n=1000)
## Test if sample size is the issue
#--------------------------------

cat("\n=== MODEL 1 FAKE: Same model with n=1000 fake data ===\n")

# Fit the same model with fake data
model1_fake_fit <- brm(
  model1_formula,
  data = df_fake_mod,  # Using fake data instead
  prior = priors1,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 456,  # Different seed
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  file = "Models/working_model1_fake_20251229",
  file_refit = "on_change"
)

summary(model1_fake_fit)

# Check convergence on fake data
cat("\nFake data convergence diagnostics:\n")
cat("Divergent transitions:", sum(nuts_params(model1_fake_fit)$Value), "\n")
cat("Max Rhat:", max(rhat(model1_fake_fit), na.rm = TRUE), "\n")
cat("Min ESS ratio:", min(neff_ratio(model1_fake_fit), na.rm = TRUE), "\n")

# Compare real vs fake
cat("\n=== COMPARISON: Real (n=35) vs Fake (n=1000) ===\n")
cat("Real data - Divergences:", sum(nuts_params(model1_fit)$Value), "\n")
cat("Fake data - Divergences:", sum(nuts_params(model1_fake_fit)$Value), "\n")
cat("Real data - Max Rhat:", round(max(rhat(model1_fit), na.rm = TRUE), 4), "\n")
cat("Fake data - Max Rhat:", round(max(rhat(model1_fake_fit), na.rm = TRUE), 4), "\n")

#--------------------------------

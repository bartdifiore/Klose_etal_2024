#--------------------------------
## Option 3c: Bayesian Latent Variable SEM using brms - Brown's Approach
## Modeling stream quality as a latent state influenced by burn status
## and drought, which predicts trout presence/absence
##
## Based on approach from:
## - Scott Brown: https://nthobservatory.github.io/latentvarsbook/brms-2pl.html
## - Fixes latent variance to 1 (standard normal) rather than fixing first loading
## - Uses directional prior (exponential) on one indicator to prevent parameter switching
## - Integrated multivariate approach combining gaussian and bernoulli families
#--------------------------------

# Load libraries
library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggdag)
library(dagitty)

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
## Conceptual model structure (DAG)
#--------------------------------

# The full model structure:
# Burned status + Wet/Dry --> Latent "Stream Quality"
# Latent Stream Quality --> Environmental indicators (conductivity, depth, DO, thermal)
# Latent Stream Quality --> Trout Presence/Absence

# Create DAG for visualization
dag <- dagify(
  Quality ~ Burned + WetDry,
  Conductivity ~ Quality,
  Depth ~ Quality,
  DO ~ Quality,
  Thermal ~ Quality,
  Trout ~ Quality,

  exposure = c("Burned", "WetDry"),
  outcome = "Trout",
  latent = "Quality"
)

# Plot DAG
ggdag(dag) +
  theme_dag() +
  labs(title = "Conceptual Model: Fire and Drought Effects on Trout via Stream Quality")

#--------------------------------
## Define formula components (used across multiple models)
#--------------------------------

# Environmental indicators caused by latent stream quality
# These are the same across all models
bf_conduct <- bf(conduct_log_scaled ~ 0 + mi(stream_quality))
bf_depth <- bf(max_depth_scaled ~ 0 + mi(stream_quality))
bf_do <- bf(do_scaled ~ 0 + mi(stream_quality))
bf_thermal <- bf(thermal_scaled ~ 0 + mi(stream_quality))

# Trout presence predicted by latent stream quality
# Uses bernoulli family for binary outcome
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
  prior(exponential(1.8), class = "b", coef = "mistream_quality", resp = "thermalscaled", lb = 0) +  # DIRECTIONAL (lb=0 to suppress warning)
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
  iter = 4000,        # Increased from 2000
  warmup = 2000,      # Increased from 1000
  seed = 123,
  control = list(adapt_delta = 0.95,    # Increased from default 0.8
                 max_treedepth = 12),   # Increased from default 10
  file = "Models/brms_model1_brown_indicators_only",
  file_refit = "always"
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

#--------------------------------
## Model 2: Indicators + Latent Variable + Predictors of Latent Variable
## Burn status and drought predict stream quality
#--------------------------------

cat("\n=== MODEL 2: Indicators + Latent Variable + Exogenous Predictors (Brown's Approach) ===\n")

# Latent variable predicted by burn status and drought
bf_quality2 <- bf(stream_quality | mi() ~ burned + wet_dry + 0)

# Combine components (same indicators as Model 1)
model2_formula <-
  bf_quality2 +
  bf_conduct +
  bf_depth +
  bf_do +
  bf_thermal +
  set_rescor(FALSE)

# Same priors as Model 1
priors2 <-
  prior(normal(0, 1), class = "sigma", resp = "streamquality") +
  prior(exponential(1.8), class = "b", coef = "mistream_quality", resp = "thermalscaled", lb = 0) +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "conductlogscaled") +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "maxdepthscaled") +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "doscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "conductlogscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "maxdepthscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "doscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "thermalscaled")

# Fit the model
model2_fit <- brm(
  model2_formula,
  data = df_mod,
  prior = priors2,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  file = "Models/brms_model2_brown_with_predictors",
  file_refit = "always"
)

summary(model2_fit)

# Check effects of burn and drought on latent stream quality
cat("\nModel 2 - Effects on Latent Stream Quality:\n")
posterior_summary(model2_fit,
                 variable = c("b_streamquality_burnedU", "b_streamquality_wet_dryW"))

# Visualize effects
mcmc_areas(model2_fit,
           pars = c("b_streamquality_burnedU", "b_streamquality_wet_dryW"),
           prob = 0.95) +
  labs(title = "Model 2: Effects of Burn Status and Drought on Stream Quality")

#--------------------------------
## Model 3: Indicators + Latent Variable + Trout Outcome
## Stream quality predicts trout presence (no exogenous predictors)
#--------------------------------

cat("\n=== MODEL 3: Indicators + Latent Variable + Trout Outcome (Brown's Approach) ===\n")

# Latent variable with no predictors
bf_quality3 <- bf(stream_quality | mi() ~ 0)

# Combine indicators AND trout outcome
# This is the key: combining gaussian (indicators) and bernoulli (trout) in one model
model3_formula <-
  bf_quality3 +
  bf_conduct +
  bf_depth +
  bf_do +
  bf_thermal +
  bf_trout +  # ADD binary outcome
  set_rescor(FALSE)

# Priors for Model 3
# CRITICAL: Add regularizing prior on trout loading due to separation
# Data shows quasi-complete separation (max quality when present = 0.184,
# max when absent = 0.988), which causes coefficient to blow up to -∞
# Using student_t(3, 0, 2.5) as recommended for regularizing logistic regression
priors3 <-
  prior(normal(0, 1), class = "sigma", resp = "streamquality") +
  prior(exponential(1.8), class = "b", coef = "mistream_quality", resp = "thermalscaled", lb = 0) +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "conductlogscaled") +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "maxdepthscaled") +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "doscaled") +
  prior(student_t(3, 0, 2.5), class = "b", coef = "mistream_quality", resp = "troutpresent") +  # REGULARIZING PRIOR
  prior(gamma(1, 0.5), class = "sigma", resp = "conductlogscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "maxdepthscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "doscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "thermalscaled")

# Fit the model
model3_fit <- brm(
  model3_formula,
  data = df_mod,
  prior = priors3,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  file = "Models/brms_model3_brown_with_trout",
  file_refit = "always"
)

summary(model3_fit)

# Check effect of stream quality on trout presence
cat("\nModel 3 - Effect of Stream Quality on Trout:\n")
posterior_summary(model3_fit,
                 variable = c("bsp_troutpresent_mistream_quality"))

# Visualize effect on trout
mcmc_areas(model3_fit,
           pars = c("bsp_troutpresent_mistream_quality"),
           prob = 0.95) +
  labs(title = "Model 3: Effect of Stream Quality on Trout Presence")

#--------------------------------
## Model 4: Full Model
## Indicators + Latent Variable + Predictors of Latent + Trout Outcome
#--------------------------------

cat("\n=== MODEL 4: Full Model (All Components - Brown's Approach) ===\n")

# Latent variable predicted by burn and drought
bf_quality4 <- bf(stream_quality | mi() ~ burned + wet_dry + 0)

# Combine ALL components: predictors → latent → indicators + trout
model4_formula <-
  bf_quality4 +
  bf_conduct +
  bf_depth +
  bf_do +
  bf_thermal +
  bf_trout +
  set_rescor(FALSE)

# Same priors as Model 3 (includes regularizing prior on trout loading)
priors4 <-
  prior(normal(0, 1), class = "sigma", resp = "streamquality") +
  prior(exponential(1.8), class = "b", coef = "mistream_quality", resp = "thermalscaled", lb = 0) +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "conductlogscaled") +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "maxdepthscaled") +
  prior(normal(0, 5), class = "b", coef = "mistream_quality", resp = "doscaled") +
  prior(student_t(3, 0, 2.5), class = "b", coef = "mistream_quality", resp = "troutpresent") +  # REGULARIZING PRIOR
  prior(gamma(1, 0.5), class = "sigma", resp = "conductlogscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "maxdepthscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "doscaled") +
  prior(gamma(1, 0.5), class = "sigma", resp = "thermalscaled")

# Fit the model
model4_fit <- brm(
  model4_formula,
  data = df_mod,
  prior = priors4,
  chains = 4,
  iter = 4000,
  warmup = 2000,
  seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  file = "Models/brms_model4_brown_full",
  file_refit = "always"
)

summary(model4_fit)

# Extract all key parameters
cat("\nModel 4 - Effects of Burn/Drought on Stream Quality:\n")
posterior_summary(model4_fit,
                 variable = c("b_streamquality_burnedU", "b_streamquality_wet_dryW"))

cat("\nModel 4 - Factor Loadings:\n")
posterior_summary(model4_fit,
                 variable = c("bsp_conductlogscaled_mistream_quality",
                             "bsp_maxdepthscaled_mistream_quality",
                             "bsp_doscaled_mistream_quality",
                             "bsp_thermalscaled_mistream_quality"))

cat("\nModel 4 - Effect of Stream Quality on Trout:\n")
posterior_summary(model4_fit,
                 variable = c("bsp_troutpresent_mistream_quality"))

# Check latent variance (should be close to 1)
cat("\nModel 4 - Latent Variance (should be ~1):\n")
posterior_summary(model4_fit,
                 variable = "sigma_streamquality")

#--------------------------------
## Model Diagnostics (Model 4 - Full Model)
#--------------------------------

cat("\n=== DIAGNOSTICS FOR MODEL 4 ===\n")

# Posterior predictive checks for continuous indicators
pp_check(model4_fit, resp = "conductlogscaled", ndraws = 100) +
  labs(title = "PP Check: Conductivity")

pp_check(model4_fit, resp = "maxdepthscaled", ndraws = 100) +
  labs(title = "PP Check: Max Depth")

pp_check(model4_fit, resp = "doscaled", ndraws = 100) +
  labs(title = "PP Check: Dissolved Oxygen")

pp_check(model4_fit, resp = "thermalscaled", ndraws = 100) +
  labs(title = "PP Check: Thermal Index")

# Posterior predictive check for binary outcome (trout)
pp_check(model4_fit, resp = "troutpresent", ndraws = 100, type = "bars") +
  labs(title = "PP Check: Trout Presence (Binary)")

# Check convergence diagnostics
# Rhat should be < 1.01, ESS should be > 400
cat("\nConvergence diagnostics (check Rhat < 1.01, ESS > 400):\n")
summary(model4_fit)

#--------------------------------
## Comprehensive Visualization of Model 4
#--------------------------------

# Extract all parameters for visualization
params_model4 <- model4_fit %>%
  gather_draws(b_streamquality_burnedU,
               b_streamquality_wet_dryW,
               bsp_conductlogscaled_mistream_quality,
               bsp_maxdepthscaled_mistream_quality,
               bsp_doscaled_mistream_quality,
               bsp_thermalscaled_mistream_quality,
               bsp_troutpresent_mistream_quality)

# Create labels for better interpretation
param_labels <- c(
  "b_streamquality_burnedU" = "Unburned → Quality",
  "b_streamquality_wet_dryW" = "Wet → Quality",
  "bsp_conductlogscaled_mistream_quality" = "Quality → Conductivity",
  "bsp_maxdepthscaled_mistream_quality" = "Quality → Depth",
  "bsp_doscaled_mistream_quality" = "Quality → DO",
  "bsp_thermalscaled_mistream_quality" = "Quality → Thermal",
  "bsp_troutpresent_mistream_quality" = "Quality → Trout"
)

# Comprehensive plot
ggplot(params_model4, aes(x = .value, y = .variable)) +
  stat_halfeye() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  scale_y_discrete(labels = param_labels) +
  labs(title = "Model 4: All Parameter Estimates (Brown's Approach)",
       subtitle = "Fire and Drought → Stream Quality → Indicators + Trout Presence",
       x = "Estimate (95% Credible Interval)",
       y = "") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1))

#--------------------------------
## Model Comparison
#--------------------------------

# Compare models using LOO (Leave-One-Out Cross-Validation)
# Note: Can only compare models fit to the same data
# Models 1 and 2 don't include trout, so compare separately

cat("\n=== MODEL COMPARISON ===\n")

# Compare Models 3 and 4 (both include trout)
loo3 <- loo(model3_fit, resp = c("conductlogscaled", "maxdepthscaled",
                                  "doscaled", "thermalscaled", "troutpresent"))
loo4 <- loo(model4_fit, resp = c("conductlogscaled", "maxdepthscaled",
                                  "doscaled", "thermalscaled", "troutpresent"))

cat("\nComparing Model 3 vs Model 4:\n")
loo_compare(loo3, loo4)

# Interpretation: Negative elpd_diff favors Model 4 if predictors improve fit

#--------------------------------
## Extract Predicted Probabilities for Trout (Model 4)
#--------------------------------

# Get predicted probabilities of trout presence
predictions_trout <- df_mod %>%
  add_epred_draws(model4_fit, resp = "troutpresent") %>%
  median_qi(.epred)

# Plot predicted vs observed
ggplot(predictions_trout, aes(x = as.factor(trout_present), y = .epred)) +
  geom_boxplot(aes(fill = as.factor(trout_present))) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(title = "Model 4: Predicted Probability of Trout Presence",
       x = "Observed Trout Presence (0 = Absent, 1 = Present)",
       y = "Predicted Probability",
       fill = "Observed") +
  theme_minimal()

#--------------------------------
## Extract and Visualize Latent Stream Quality (Model 4)
#--------------------------------

# Extract posterior estimates of latent stream quality for each observation
latent_quality <- model4_fit %>%
  spread_draws(Ymi_streamquality[obs]) %>%
  group_by(obs) %>%
  median_qi(Ymi_streamquality)

# Add back to original data
df_mod$quality_posterior_median <- latent_quality$Ymi_streamquality
df_mod$quality_lower <- latent_quality$.lower
df_mod$quality_upper <- latent_quality$.upper

# Visualize latent quality by burn status and drought
ggplot(df_mod, aes(x = interaction(burned, wet_dry), y = quality_posterior_median)) +
  geom_boxplot(aes(fill = interaction(burned, wet_dry))) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
  labs(title = "Model 4: Latent Stream Quality by Burn Status and Drought (Brown's Approach)",
       x = "Burn Status × Drought Status",
       y = "Latent Stream Quality (posterior median)",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualize relationship between latent quality and trout presence
ggplot(df_mod, aes(x = quality_posterior_median, y = trout_present)) +
  geom_point(alpha = 0.5, position = position_jitter(height = 0.05)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "blue") +
  labs(title = "Model 4: Latent Stream Quality vs Trout Presence",
       x = "Latent Stream Quality (posterior median)",
       y = "Trout Presence (0/1)") +
  theme_minimal()

#--------------------------------
## Compare Brown vs Byrnes Approaches
#--------------------------------

cat("\n=== COMPARISON: BROWN'S vs BYRNES' APPROACH ===\n")
cat("\nKey differences in identifiability strategies:\n")
cat("\nBrown's Approach (THIS FILE):\n")
cat("  - Latent variance: Fixed to 1 via normal(0, 1) prior\n")
cat("  - Factor loadings: All freely estimated\n")
cat("  - Directional prior: exponential(1.8) on thermal_index loading\n")
cat("  - Other loadings: normal(0, 5) weakly informative priors\n")
cat("  - Prevents parameter switching via directional prior\n")
cat("\nByrnes' Approach (File 6):\n")
cat("  - Latent variance: Freely estimated with gamma(11, 11) prior\n")
cat("  - Factor loadings: First loading fixed to 1 via constant(1) prior\n")
cat("  - Other loadings: Implicitly estimated\n")
cat("  - May suffer from label switching if all parameters symmetric\n")
cat("\nConvergence comparison:\n")
cat("  - Brown's approach may have better convergence due to directional prior\n")
cat("  - Check Rhat and ESS values across both approaches\n")
cat("  - Look for bimodal posteriors in Byrnes approach (sign of switching)\n")

#--------------------------------
## Key findings and interpretation
#--------------------------------

cat("\n=== SUMMARY ===\n")
cat("\nFour models fitted using Brown's identifiability approach:\n")
cat("Model 1: Latent variable indicated by 4 environmental variables\n")
cat("Model 2: Model 1 + burn and drought predict latent quality\n")
cat("Model 3: Model 1 + latent quality predicts trout presence\n")
cat("Model 4: Full model (burn/drought → quality → indicators + trout)\n")
cat("\nAll models use integrated brms approach with mi() notation\n")
cat("Binary outcome (trout) successfully combined with gaussian indicators\n")
cat("\nIdentification strategy (Brown's approach):\n")
cat("  - Latent variance fixed to 1\n")
cat("  - Directional prior on thermal_index prevents parameter switching\n")
cat("  - Should improve convergence compared to symmetric priors\n")
cat("\nInterpretation:\n")
cat("- Factor loadings show which environmental variables indicate stream quality\n")
cat("- Effects of burn/drought show how disturbances affect stream quality\n")
cat("- Effect of quality on trout shows whether stream quality predicts trout presence\n")
cat("- Model 4 provides the complete causal pathway: disturbance → quality → trout\n")

#--------------------------------

#--------------------------------
## Fit Full Stan latent variable model WITH INTERACTION
## Using rstan
## Includes: 6 indicators, burn/drought predictors + interaction, trout outcome
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
    # Response variable
    trout_present = as.integer(ifelse(trout.present..absent == "P", 1, 0)),

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
  filter(!is.na(trout_present),
         !is.na(conduct_log_scaled),
         !is.na(max_depth_scaled),
         !is.na(do_scaled),
         !is.na(thermal_scaled),
         !is.na(canopy_logit_scaled),
         !is.na(q_log_scaled),
         !is.na(burned_coded),
         !is.na(wet_coded))

cat("Sample size:", nrow(df_mod), "\n")

#--------------------------------
## Prepare data for Stan
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
  # Outcome
  trout = df_mod$trout_present
)

#--------------------------------
## Compile and fit Stan model
#--------------------------------

# Compile the model
model <- stan_model("Code/stream_quality_full_interaction.stan")

# Fit the model
fit <- sampling(
  object = model,
  data = stan_data,
  chains = 4,
  cores = 4,
  warmup = 2000,
  iter = 4000,  # Total iterations = warmup + sampling
  seed = 123,
  control = list(adapt_delta = 0.95, max_treedepth = 12),
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

cat("\n=== EFFECTS ON STREAM QUALITY (WITH INTERACTION) ===\n")
print(fit, pars = c("alpha_quality", "beta_burned", "beta_wet", "beta_interaction"))

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
                pars = c("beta_conduct", "beta_depth", "beta_do",
                        "beta_thermal", "beta_canopy", "beta_q"),
                prob = 0.95) +
  labs(title = "Factor Loadings (Stream Quality Indicators)",
       subtitle = "How strongly each indicator reflects stream quality")

print(p1)

# Plot effects of burn and drought on stream quality (WITH INTERACTION)
p2 <- mcmc_areas(draws,
                pars = c("beta_burned", "beta_wet", "beta_interaction"),
                prob = 0.95) +
  labs(title = "Effects of Disturbance on Stream Quality (with Interaction)",
       subtitle = "Burned vs Unburned, Wet vs Dry, Burned × Wet")

print(p2)

# Plot effect of stream quality on trout
p3 <- mcmc_areas(draws,
                pars = c("beta_trout"),
                prob = 0.95) +
  labs(title = "Effect of Stream Quality on Trout Presence",
       subtitle = "Logistic regression coefficient")

print(p3)

# Calculate implied group means from the interaction model
posterior <- as.data.frame(fit)

group_means <- data.frame(
  Group = c("Unburned-Dry (Reference)", "Burned-Dry", "Unburned-Wet", "Burned-Wet"),
  Mean = c(
    mean(posterior$alpha_quality),
    mean(posterior$alpha_quality + posterior$beta_burned),
    mean(posterior$alpha_quality + posterior$beta_wet),
    mean(posterior$alpha_quality + posterior$beta_burned + posterior$beta_wet + posterior$beta_interaction)
  ),
  SD = c(
    sd(posterior$alpha_quality),
    sd(posterior$alpha_quality + posterior$beta_burned),
    sd(posterior$alpha_quality + posterior$beta_wet),
    sd(posterior$alpha_quality + posterior$beta_burned + posterior$beta_wet + posterior$beta_interaction)
  )
)

cat("\n=== IMPLIED GROUP MEANS FOR STREAM QUALITY ===\n")
print(group_means, digits = 3)

#--------------------------------
## Save results
#--------------------------------

# Save the fitted model
saveRDS(fit, file = "Models/stan_model_full_interaction_fit.rds")

# Save summary
summary_df <- as.data.frame(summary(fit)$summary)
write.csv(summary_df, "Models/stan_model_full_interaction_summary.csv", row.names = TRUE)

cat("\nModel fitting complete!\n")
cat("Model saved to: Models/stan_model_full_interaction_fit.rds\n")
cat("Summary saved to: Models/stan_model_full_interaction_summary.csv\n")

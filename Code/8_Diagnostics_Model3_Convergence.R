#--------------------------------
## Diagnostic Script: Investigating Model 3 Convergence Issues
## Problem: bsp_troutpresent_mistream_quality has Rhat = 2.81, Tail_ESS = 13
## This indicates SEVERE non-convergence
#--------------------------------

library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)

# Load the fitted models
model1_fit <- readRDS("Models/brms_model1_brown_indicators_only.rds")
model3_fit <- readRDS("Models/brms_model3_brown_with_trout.rds")

# Load data
df <- read.csv("Data/cleaned_20251130.csv")

df_mod <- df %>%
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
  ) %>%
  filter(!is.na(trout_present),
         !is.na(conduct_log_scaled),
         !is.na(max_depth_scaled),
         !is.na(do_scaled),
         !is.na(thermal_scaled))

cat("=== DIAGNOSTIC 1: CONVERGENCE METRICS ===\n\n")

# Extract full diagnostics for the problematic parameter
cat("Full diagnostics for bsp_troutpresent_mistream_quality:\n")
print(posterior_summary(model3_fit,
                       variable = c("bsp_troutpresent_mistream_quality")))

cat("\nAll parameters (check for other problems):\n")
print(summary(model3_fit))

#--------------------------------
cat("\n\n=== DIAGNOSTIC 2: VISUAL CHAIN DIAGNOSTICS ===\n\n")

# Trace plots - should look like "hairy caterpillars"
# If chains are stuck or wandering, you'll see it here
trace_plot <- mcmc_trace(model3_fit,
                        pars = c("bsp_troutpresent_mistream_quality",
                                "bsp_conductlogscaled_mistream_quality",
                                "bsp_thermalscaled_mistream_quality"),
                        facet_args = list(ncol = 1))

print(trace_plot +
  labs(title = "Trace Plots: Check for chain mixing",
       subtitle = "Should look like overlapping hairy caterpillars. If separated = bad convergence"))

# Density plots - check for bimodality or weird shapes
dens_plot <- mcmc_dens_overlay(model3_fit,
                              pars = c("bsp_troutpresent_mistream_quality"))

print(dens_plot +
  labs(title = "Density Overlay: All 4 Chains for Trout Parameter",
       subtitle = "Should overlap. If separated or bimodal = convergence failure"))

#--------------------------------
cat("\n\n=== DIAGNOSTIC 3: CHECK FOR SEPARATION ===\n\n")

# Extract latent quality from Model 1 (which doesn't include trout)
# This gives us an independent estimate of quality to check separation
latent_quality_m1 <- model1_fit %>%
  spread_draws(Ymi_streamquality[obs]) %>%
  group_by(obs) %>%
  median_qi(Ymi_streamquality)

df_mod$quality_m1 <- latent_quality_m1$Ymi_streamquality

# Check if trout presence/absence perfectly or nearly perfectly separates by quality
cat("Summary of latent quality by trout presence:\n")
print(df_mod %>%
  group_by(trout_present) %>%
  summarise(
    n = n(),
    mean_quality = mean(quality_m1),
    sd_quality = sd(quality_m1),
    min_quality = min(quality_m1),
    max_quality = max(quality_m1)
  ))

# Check overlap - if min(present) > max(absent), we have complete separation
cat("\n\nChecking for complete separation:\n")
quality_present <- df_mod %>% filter(trout_present == 1) %>% pull(quality_m1)
quality_absent <- df_mod %>% filter(trout_present == 0) %>% pull(quality_m1)

cat("Min quality where trout PRESENT:", min(quality_present), "\n")
cat("Max quality where trout ABSENT:", max(quality_absent), "\n")
cat("Overlap:", min(quality_present) <= max(quality_absent), "\n")
cat("Gap size:", min(quality_present) - max(quality_absent), "\n")

# Quartile analysis
df_mod <- df_mod %>%
  mutate(quality_quartile = cut(quality_m1,
                                breaks = quantile(quality_m1, probs = 0:4/4),
                                labels = c("Q1_Low", "Q2", "Q3", "Q4_High"),
                                include.lowest = TRUE))

cat("\n\nTrout presence by quality quartile:\n")
print(table(Quartile = df_mod$quality_quartile, Trout = df_mod$trout_present))

cat("\nProportions (rows = quartiles):\n")
print(round(prop.table(table(df_mod$quality_quartile, df_mod$trout_present), margin = 1), 3))

# Visualization
sep_plot <- ggplot(df_mod, aes(x = quality_m1, y = trout_present)) +
  geom_point(alpha = 0.5, position = position_jitter(height = 0.05, width = 0)) +
  geom_rug(aes(color = as.factor(trout_present)), sides = "b", alpha = 0.5) +
  geom_vline(xintercept = min(quality_present), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = max(quality_absent), linetype = "dashed", color = "red") +
  annotate("text", x = min(quality_present), y = 0.9,
           label = "Min (present)", angle = 90, vjust = -0.5, color = "blue") +
  annotate("text", x = max(quality_absent), y = 0.1,
           label = "Max (absent)", angle = 90, vjust = 1.5, color = "red") +
  labs(title = "Separation Check: Latent Quality vs Trout Presence",
       subtitle = "Complete separation = dashed lines don't overlap",
       x = "Latent Stream Quality (from Model 1)",
       y = "Trout Present (0/1)",
       color = "Trout") +
  theme_minimal()

print(sep_plot)

#--------------------------------
cat("\n\n=== DIAGNOSTIC 4: CHECK PRIOR INFLUENCE ===\n\n")

# Sample from prior only to see if prior is informative enough
cat("Sampling from prior for trout parameter...\n")
cat("(This may take a moment)\n")

model3_prior <- brm(
  model3_formula,
  data = df_mod,
  prior = priors3,
  chains = 2,
  iter = 1000,
  sample_prior = "only",  # Sample ONLY from prior
  seed = 456
)

cat("\nPrior samples for trout loading:\n")
prior_samples <- as_draws_df(model3_prior) %>%
  select(matches("bsp_troutpresent_mistream_quality"))

cat("Prior mean:", mean(prior_samples[[1]]), "\n")
cat("Prior SD:", sd(prior_samples[[1]]), "\n")
cat("Prior 95% interval:", quantile(prior_samples[[1]], c(0.025, 0.975)), "\n")

cat("\nIf prior is too wide and data has separation, posterior can wander to extreme values\n")

#--------------------------------
cat("\n\n=== DIAGNOSTIC 5: SIMPLE GLM CHECK ===\n\n")

# Fit a simple GLM with latent quality as predictor
# This checks if the data itself causes issues
cat("Fitting simple frequentist GLM: trout ~ quality_m1\n")
glm_check <- glm(trout_present ~ quality_m1,
                 data = df_mod,
                 family = binomial())

cat("\nGLM summary:\n")
print(summary(glm_check))

cat("\nIf GLM coefficient is huge or has huge SE, separation is the problem\n")

#--------------------------------
cat("\n\n=== RECOMMENDATIONS ===\n\n")

cat("Based on diagnostics above:\n\n")

cat("1. If SEPARATION is detected (no overlap in quality ranges):\n")
cat("   → Use stronger prior on trout loading: normal(0, 2) instead of default\n")
cat("   → Consider removing extreme observations\n")
cat("   → Use weakly informative prior: student_t(3, 0, 2.5)\n\n")

cat("2. If chains are WANDERING (trace plot not mixing):\n")
cat("   → Increase adapt_delta to 0.99\n")
cat("   → Increase iterations to 8000\n")
cat("   → Try different initialization values\n\n")

cat("3. If MULTIMODALITY (density plot has 2+ peaks):\n")
cat("   → Model has identifiability issues\n")
cat("   → May need to add prior on trout loading direction\n")
cat("   → Consider exponential prior if you know direction\n\n")

cat("4. If PRIOR is too weak:\n")
cat("   → Add informative prior: prior(normal(0, 2), class = 'b', resp = 'troutpresent')\n\n")

#--------------------------------

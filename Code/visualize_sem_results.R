#--------------------------------
## Publication-Quality Figures for SEM Model Results
## Model: stan_model_full_hurdle_fit.rds
#--------------------------------

library(tidyverse)
library(rstan)
library(bayesplot)
library(ggdist)
library(patchwork)
library(tidybayes)

#--------------------------------
## Load model and data
#--------------------------------

# Load fitted model
fit <- readRDS("Models/stan_model_full_hurdle_wintercept_v2_fit.rds")

# Load original data
df <- read.csv("Data/Data_20240408.csv")

df_mod <- df %>%
  mutate(
    # Response variable
    trout_count = as.integer(total.trout),

    # Transform continuous predictors (scaled for modeling)
    max_depth_scaled = as.numeric(scale(Max.depth.m)),
    do_scaled = as.numeric(scale(as.numeric(Point.Minimum.DO.mg.L))),
    conduct_log_scaled = as.numeric(scale(log(Conductivity.uS.cm.))),
    canopy_logit_scaled = as.numeric(scale(qlogis(Average.canopy.cover/100))),
    thermal_scaled = as.numeric(scale(Thermal.index)),
    q_log_scaled = as.numeric(scale(log(`Q.estimate..m3.s.`))),

    # Categorical predictors
    burned_coded = as.numeric(ifelse(`Burned..B..vs..unburned..U.` == "B", 1, 0)),
    wet_coded = as.numeric(ifelse(`Wet.or.dry.in.2016` == "W", 1, 0)),

    # Factor versions for plotting
    burned = `Burned..B..vs..unburned..U.`,
    wet_dry = `Wet.or.dry.in.2016`
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

# Extract posterior draws
draws <- as.array(fit)

#--------------------------------
## Figure 1: Posterior densities for indicator loadings (betas)
#--------------------------------

# Extract factor loading parameters
loading_params <- c("beta_conduct", "beta_depth", "beta_do",
                    "beta_thermal", "beta_canopy", "beta_q")

# Create nice labels
loading_labels <- c(
  "beta_conduct" = "Conductivity",
  "beta_depth" = "Depth",
  "beta_do" = "Dissolved Oxygen",
  "beta_thermal" = "Thermal Index",
  "beta_canopy" = "Canopy Cover",
  "beta_q" = "Discharge (Q)"
)

# Extract posterior samples and convert to long format
loading_draws <- as.data.frame(fit, pars = loading_params) %>%
  pivot_longer(everything(), names_to = "parameter", values_to = "value") %>%
  mutate(indicator = loading_labels[parameter],
         indicator = factor(indicator, levels = loading_labels))

# Calculate summary statistics for each parameter
loading_summary <- loading_draws %>%
  group_by(indicator) %>%
  summarize(
    median = median(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    prob_positive = mean(value > 0)
  )

# Create posterior density plot
fig1 <- ggplot(loading_draws, aes(x = value, y = indicator)) +
  stat_halfeye(
    .width = c(0.95, 0.80),
    point_interval = "median_qi",
    fill = "steelblue",
    alpha = 0.7,
    normalize = "xy"
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red",
             linewidth = 0.5) +
  labs(
    x = "Factor Loading (β)",
    y = "Environmental Indicator",
    title = "Factor Loadings: Stream Quality → Indicators",
    subtitle = "Points show median; thick bars = 80% CI; thin bars = 95% CI"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold"),
    panel.grid.major.x = element_line(color = "grey90")
  )

print(fig1)
ggsave("Figures/fig1_factor_loadings.png", fig1,
       width = 8, height = 6, dpi = 300)
ggsave("Figures/fig1_factor_loadings.pdf", fig1,
       width = 8, height = 6)

#--------------------------------
## Figure 2: Conditional effects of Burn and Wet/Dry on Latent State
#--------------------------------

# Extract stream quality estimates for each observation
stream_quality_samples <- as.matrix(fit,
  pars = paste0("stream_quality[", 1:nrow(df_mod), "]"))

# Calculate median for each observation
df_mod$stream_quality_median <- apply(stream_quality_samples, 2, median)

beta_estimates <- fit %>% 
  gather_draws(mu_unburned_dry, mu_burned_dry, mu_unburned_wet, mu_burned_wet) %>%
  median_qi() %>%
  mutate(cat = case_when(.variable == "mu_burned_dry" ~ "B-D", 
                         .variable == "mu_burned_wet" ~ "B-W", 
                         .variable == "mu_unburned_dry" ~ "U-D", 
                         .variable == "mu_unburned_wet" ~ "U-W")) 

forplot <- df_mod %>%
  select(burned, wet_dry, stream_quality_median) %>%
  mutate(cat= paste(burned, "-", wet_dry, sep = ""), 
         cat_final = case_when(cat == "B-D" ~ "Burned Dry", 
                               cat == "B-W" ~ "Burned Wet", 
                               cat == "U-W" ~ "Unburned Wet", 
                               cat == "U-D" ~ "Unburned Dry")) %>%
  left_join(beta_estimates)
  

ggplot(forplot, aes(x = cat_final, y = stream_quality_median,
                           color = burned, fill = burned)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_jitter(size = 2.5, alpha = 0.6, width = 0.15, pch = 21) +
  geom_pointinterval(aes(y = .value, ymin = .lower, ymax = .upper, x = cat_final), pch = 1)+
  # scale_color_manual(values = c("B" = "darkorange", "U" = "forestgreen")) +
  scale_fill_manual(values = c("B" = "darkorange", "U" = "forestgreen")) +
  labs(
    x = "Burn and Drought Status",
    y = "Latent Stream Quality",
    title = "Effect of Burn and Drought Status on Stream Quality"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Prepare data for violin plots of full posteriors by group
# Extract full posterior for stream_quality and reshape for plotting
stream_quality_posterior_long <- data.frame()

for (i in 1:nrow(df_mod)) {
  # Get posterior samples for this site
  site_posterior <- stream_quality_samples[, i]

  # Create data frame with group info
  site_df <- data.frame(
    stream_quality = site_posterior,
    site_id = i,
    burned = df_mod$burned[i],
    wet_dry = df_mod$wet_dry[i]
  )

  stream_quality_posterior_long <- rbind(stream_quality_posterior_long, site_df)
}

# Plot A: Effect of Burn Status
fig2a <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  # Violin plot of full posteriors for all sites in each group
  geom_violin(data = stream_quality_posterior_long,
              aes(x = burned, y = stream_quality, fill = burned),
              alpha = 0.3, draw_quantiles = c(0.25, 0.5, 0.75)) +
  # Overlay median points for each site
  geom_jitter(data = df_mod,
              aes(x = burned, y = stream_quality_median, color = burned),
              size = 2.5, alpha = 0.6, width = 0.15) +
  scale_color_manual(values = c("B" = "darkorange", "U" = "forestgreen")) +
  scale_fill_manual(values = c("B" = "darkorange", "U" = "forestgreen")) +
  labs(
    x = "Burn Status",
    y = "Latent Stream Quality",
    title = "Effect of Burn Status on Stream Quality"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Plot B: Effect of Wet/Dry
fig2b <- ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  # Violin plot of full posteriors for all sites in each group
  geom_violin(data = stream_quality_posterior_long,
              aes(x = wet_dry, y = stream_quality, fill = wet_dry),
              alpha = 0.3, draw_quantiles = c(0.25, 0.5, 0.75)) +
  # Overlay median points for each site
  geom_jitter(data = df_mod,
              aes(x = wet_dry, y = stream_quality_median, color = wet_dry),
              size = 2.5, alpha = 0.6, width = 0.15) +
  scale_color_manual(values = c("W" = "dodgerblue", "D" = "brown")) +
  scale_fill_manual(values = c("W" = "dodgerblue", "D" = "brown")) +
  labs(
    x = "Wet/Dry Status",
    y = "Latent Stream Quality",
    title = "Effect of Wet/Dry on Stream Quality"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.title = element_text(face = "bold"),
    legend.position = "none"
  )

# Combine plots side by side
fig2 <- fig2a | fig2b

fig2 <- fig2 +
  plot_annotation(
    title = "Conditional Effects of Disturbance on Stream Quality",
    subtitle = "Additive model: no interaction between burn and wet/dry status",
    theme = theme(plot.title = element_text(face = "bold", size = 14))
  )

print(fig2)
ggsave("Figures/fig2_conditional_effects.png", fig2,
       width = 10, height = 5, dpi = 300)
ggsave("Figures/fig2_conditional_effects.pdf", fig2,
       width = 10, height = 5)

#--------------------------------
## Figure 3: Latent State vs Combined Trout Predictions
#--------------------------------

# Extract predictions from the model
# Trout presence probability
trout_presence_logit <- as.matrix(fit,
  pars = paste0("trout_presence_logit[", 1:nrow(df_mod), "]"))
trout_presence_prob <- plogis(trout_presence_logit)

# Trout abundance (conditional on presence)
trout_abundance_log <- as.matrix(fit,
  pars = paste0("trout_abundance_log[", 1:nrow(df_mod), "]"))
trout_abundance_mean <- exp(trout_abundance_log)

# Combined expected count = P(presence) × E(count | presence)
expected_count <- trout_presence_prob * trout_abundance_mean

# Calculate summaries
df_mod$prob_presence <- apply(trout_presence_prob, 2, median)
df_mod$expected_abundance <- apply(trout_abundance_mean, 2, median)
df_mod$expected_count <- apply(expected_count, 2, median)
df_mod$expected_count_lower <- apply(expected_count, 2,
                                      function(x) quantile(x, 0.025))
df_mod$expected_count_upper <- apply(expected_count, 2,
                                      function(x) quantile(x, 0.975))

# Create presence/absence indicator
df_mod$trout_present <- ifelse(df_mod$trout_count > 0, "Present", "Absent")

# Generate prediction ribbon across range of stream quality
stream_quality_seq <- seq(min(df_mod$stream_quality_median),
                          max(df_mod$stream_quality_median),
                          length.out = 100)

# Extract parameter samples for predictions
alpha_presence <- as.matrix(fit, pars = "alpha_trout_presence")
beta_presence <- as.matrix(fit, pars = "beta_trout_presence")
alpha_abundance <- as.matrix(fit, pars = "alpha_trout_abundance")
beta_abundance <- as.matrix(fit, pars = "beta_trout_abundance")

# Calculate predictions for each stream quality value
pred_matrix <- matrix(NA, nrow = length(stream_quality_seq), ncol = 3)
for (i in seq_along(stream_quality_seq)) {
  sq <- stream_quality_seq[i]

  # Sample predictions
  prob_pres <- plogis(alpha_presence + beta_presence * sq)
  abund_pres <- exp(alpha_abundance + beta_abundance * sq)
  exp_count <- prob_pres * abund_pres

  pred_matrix[i, 1] <- median(exp_count)
  pred_matrix[i, 2] <- quantile(exp_count, 0.025)
  pred_matrix[i, 3] <- quantile(exp_count, 0.975)
}

pred_df <- data.frame(
  stream_quality = stream_quality_seq,
  expected_count = pred_matrix[, 1],
  lower = pred_matrix[, 2],
  upper = pred_matrix[, 3]
)

# Main regression plot
fig3 <- ggplot() +
  # Prediction ribbon
  geom_ribbon(data = pred_df,
              aes(x = stream_quality, ymin = lower, ymax = upper),
              fill = "steelblue", alpha = 0.3) +
  geom_line(data = pred_df,
            aes(x = stream_quality, y = expected_count),
            color = "steelblue", linewidth = 1.5) +
  # Raw data points
  geom_point(data = df_mod,
             aes(x = stream_quality_median, y = trout_count,
                 shape = trout_present, color = trout_present),
             size = 3, alpha = 0.7) +
  scale_shape_manual(values = c("Absent" = 1, "Present" = 16)) +
  scale_color_manual(values = c("Absent" = "grey60", "Present" = "darkorange")) +
  scale_y_continuous(trans = "log1p",
                     breaks = c(0, 1, 5, 10, 25, 50, 100, 200)) +
  labs(
    x = "Latent Stream Quality",
    y = "Expected Trout Count",
    shape = "Trout Status",
    color = "Trout Status",
    title = "Effect of Stream Quality on Trout Abundance",
    subtitle = "Combined predictions from hurdle model: P(presence) × E(count | presence)\nLine shows median; ribbon shows 95% credible interval"
  ) +
  theme_classic(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    legend.position = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(fill = "white", color = "black"),
    panel.grid.major = element_line(color = "grey90")
  )

print(fig3)
ggsave("Figures/fig3_trout_vs_quality.png", fig3,
       width = 8, height = 6, dpi = 300)
ggsave("Figures/fig3_trout_vs_quality.pdf", fig3,
       width = 8, height = 6)

#--------------------------------
## Bonus Figure 3b: Separate presence and abundance effects
#--------------------------------

# Prediction data for presence
# Need to loop over stream quality values
pred_presence_matrix <- matrix(NA, nrow = length(stream_quality_seq), ncol = 3)
for (i in seq_along(stream_quality_seq)) {
  sq <- stream_quality_seq[i]
  prob_samples <- plogis(alpha_presence + beta_presence * sq)
  pred_presence_matrix[i, 1] <- median(prob_samples)
  pred_presence_matrix[i, 2] <- quantile(prob_samples, 0.025)
  pred_presence_matrix[i, 3] <- quantile(prob_samples, 0.975)
}

pred_presence <- data.frame(
  stream_quality = stream_quality_seq,
  prob = pred_presence_matrix[, 1],
  lower = pred_presence_matrix[, 2],
  upper = pred_presence_matrix[, 3]
)

# Prediction data for abundance (given presence)
pred_abundance_matrix <- matrix(NA, nrow = length(stream_quality_seq), ncol = 3)
for (i in seq_along(stream_quality_seq)) {
  sq <- stream_quality_seq[i]
  count_samples <- exp(alpha_abundance + beta_abundance * sq)
  pred_abundance_matrix[i, 1] <- median(count_samples)
  pred_abundance_matrix[i, 2] <- quantile(count_samples, 0.025)
  pred_abundance_matrix[i, 3] <- quantile(count_samples, 0.975)
}

pred_abundance <- data.frame(
  stream_quality = stream_quality_seq,
  count = pred_abundance_matrix[, 1],
  lower = pred_abundance_matrix[, 2],
  upper = pred_abundance_matrix[, 3]
)

# Plot A: Presence
fig3b_presence <- ggplot() +
  geom_ribbon(data = pred_presence,
              aes(x = stream_quality, ymin = lower, ymax = upper),
              fill = "purple", alpha = 0.3) +
  geom_line(data = pred_presence,
            aes(x = stream_quality, y = prob),
            color = "purple", linewidth = 1.5) +
  geom_point(data = df_mod,
             aes(x = stream_quality_median,
                 y = as.numeric(trout_count > 0)),
             alpha = 0.5, position = position_jitter(height = 0.02)) +
  labs(
    x = "Latent Stream Quality",
    y = "Probability of Presence",
    title = "Hurdle Part 1: Presence/Absence"
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold")
  )

# Plot B: Abundance given presence
df_mod_present <- df_mod %>% filter(trout_count > 0)

fig3b_abundance <- ggplot() +
  geom_ribbon(data = pred_abundance,
              aes(x = stream_quality, ymin = lower, ymax = upper),
              fill = "darkgreen", alpha = 0.3) +
  geom_line(data = pred_abundance,
            aes(x = stream_quality, y = count),
            color = "darkgreen", linewidth = 1.5) +
  geom_point(data = df_mod_present,
             aes(x = stream_quality_median, y = trout_count),
             alpha = 0.7, color = "darkorange", size = 3) +
  scale_y_continuous(trans = "log1p",
                     breaks = c(1, 5, 10, 25, 50, 100, 200)) +
  labs(
    x = "Latent Stream Quality",
    y = "Count (given presence)",
    title = "Hurdle Part 2: Abundance"
  ) +
  theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 11),
    axis.title = element_text(face = "bold")
  )

# Combine
fig3b <- fig3b_presence | fig3b_abundance

print(fig3b)
ggsave("Figures/fig3b_hurdle_components.png", fig3b,
       width = 10, height = 4, dpi = 300)
ggsave("Figures/fig3b_hurdle_components.pdf", fig3b,
       width = 10, height = 4)

#--------------------------------
## Supplementary Figure: Site-Level Stream Quality Estimates
#--------------------------------

# First, add site identifiers to df_mod (keeping original row order)
df_mod <- df_mod %>%
  mutate(site_name = df$site[row_number()],
         site_id = row_number())

# Prepare data with site identifiers and summary statistics
site_summary <- df_mod %>%
  arrange(stream_quality_median) %>%
  mutate(site_rank = row_number(),
         site_ordered = factor(site_name, levels = site_name))

# Reorder the posterior long data to match
stream_quality_site_posterior <- data.frame()

for (i in 1:nrow(site_summary)) {
  # Get original site index from site_id
  original_idx <- site_summary$site_id[i]

  # Get posterior samples for this site
  site_posterior <- stream_quality_samples[, original_idx]

  site_df <- data.frame(
    stream_quality = site_posterior,
    site_name = site_summary$site_name[i],
    site_ordered = site_summary$site_ordered[i],
    burned = site_summary$burned[i],
    wet_dry = site_summary$wet_dry[i]
  )

  stream_quality_site_posterior <- rbind(stream_quality_site_posterior, site_df)
}

# Calculate summary stats for plotting (using original site_id for indexing)
lower_95_vec <- apply(stream_quality_samples, 2, function(x) quantile(x, 0.025))
upper_95_vec <- apply(stream_quality_samples, 2, function(x) quantile(x, 0.975))

site_summary <- site_summary %>%
  mutate(
    lower_95 = lower_95_vec[site_id],
    upper_95 = upper_95_vec[site_id]
  )

# Create half-eye plot
fig_supp_sites <- ggplot(stream_quality_site_posterior,
                         aes(x = stream_quality, y = site_ordered)) +
  stat_halfeye(
    .width = c(0.95),
    point_interval = "median_qi",
    fill = "steelblue",
    alpha = 0.7,
    normalize = "xy",
    scale = 0.8
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.5) +
  labs(
    x = "Latent Stream Quality (ν)",
    y = "Site",
    title = "Site-Level Posterior Distributions of Stream Quality",
    subtitle = "Half-eye plots show posterior density (right) with median (point) and 95% CI (interval)\nSites ordered by median stream quality from lowest to highest"
  ) +
  theme_classic(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    plot.subtitle = element_text(size = 9),
    axis.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 7),
    panel.grid.major.x = element_line(color = "grey90")
  )

print(fig_supp_sites)
ggsave("Figures/figS_site_level_stream_quality.png", fig_supp_sites,
       width = 8, height = 10, dpi = 300)
ggsave("Figures/figS_site_level_stream_quality.pdf", fig_supp_sites,
       width = 8, height = 10)

#--------------------------------
## Summary
#--------------------------------

cat("\n=== PUBLICATION FIGURES SAVED ===\n\n")
cat("Figure 1: Factor loadings (posterior densities)\n")
cat("  - Figures/fig1_factor_loadings.png\n")
cat("  - Figures/fig1_factor_loadings.pdf\n\n")

cat("Figure 2: Conditional effects of burn/wet on stream quality\n")
cat("  - Figures/fig2_conditional_effects.png\n")
cat("  - Figures/fig2_conditional_effects.pdf\n\n")

cat("Figure 3: Stream quality vs trout (combined predictions)\n")
cat("  - Figures/fig3_trout_vs_quality.png\n")
cat("  - Figures/fig3_trout_vs_quality.pdf\n\n")

cat("Figure 3b (bonus): Separate hurdle components\n")
cat("  - Figures/fig3b_hurdle_components.png\n")
cat("  - Figures/fig3b_hurdle_components.pdf\n\n")

cat("Supplementary Figure: Site-level stream quality estimates\n")
cat("  - Figures/figS_site_level_stream_quality.png\n")
cat("  - Figures/figS_site_level_stream_quality.pdf\n\n")

cat("All figures generated successfully!\n")


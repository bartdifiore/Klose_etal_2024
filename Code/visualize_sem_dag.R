#--------------------------------
## DAG Visualization of Latent State SEM Model
## Using dagitty and ggdag packages
## Model: stan_model_full_hurdle_fit.rds
#--------------------------------

library(tidyverse)
library(dagitty)
library(ggdag)

#--------------------------------
## Load model parameter estimates
#--------------------------------

# Load summary statistics from fitted model
model_summary <- read.csv("Models/stan_model_full_hurdle_summary.csv", row.names = 1)

# Extract key parameters (median and 95% CI)
extract_param <- function(param_name) {
  row <- model_summary[param_name, ]
  list(
    median = row$X50.,
    lower = row$X2.5.,
    upper = row$X97.5.
  )
}

# Factor loadings
beta_conduct <- extract_param("beta_conduct")
beta_depth <- extract_param("beta_depth")
beta_do <- extract_param("beta_do")
beta_thermal <- extract_param("beta_thermal")
beta_canopy <- extract_param("beta_canopy")
beta_q <- extract_param("beta_q")

# Predictors of stream quality (effects coded as deviations)
# The model is: stream_quality = beta_burned * burned + beta_wet * wet
# where burned: 0=Unburned, 1=Burned
# and wet: 0=Dry, 1=Wet
beta_burned <- extract_param("beta_burned")
beta_wet <- extract_param("beta_wet")

# Calculate predicted stream quality for each level of categorical predictors
# Model: stream_quality = beta_burned * burned + beta_wet * wet
# We need to load the fitted model to get posterior samples

fit <- readRDS("Models/stan_model_full_hurdle_fit.rds")

# Load data to calculate group means
# Use EXACT same filtering as in fit_stan_model_full_hurdle.R
df <- read.csv("Data/Data_20240408.csv")
df_mod <- df %>%
  mutate(
    trout_count = as.integer(total.trout),
    max_depth_scaled = as.numeric(scale(Max.depth.m)),
    do_scaled = as.numeric(scale(as.numeric(Point.Minimum.DO.mg.L))),
    conduct_log_scaled = as.numeric(scale(log(Conductivity.uS.cm.))),
    canopy_logit_scaled = as.numeric(scale(qlogis(Average.canopy.cover/100))),
    thermal_scaled = as.numeric(scale(Thermal.index)),
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

# Extract stream quality samples
stream_quality_samples <- as.matrix(fit,
  pars = paste0("stream_quality[", 1:nrow(df_mod), "]"))

# Calculate mean stream quality for each group
unburned_wet <- which(df_mod$burned_coded == 0 & df_mod$wet_coded == 1)
unburned_dry <- which(df_mod$burned_coded == 0 & df_mod$wet_coded == 0)
burned_wet <- which(df_mod$burned_coded == 1 & df_mod$wet_coded == 1)
burned_dry <- which(df_mod$burned_coded == 1 & df_mod$wet_coded == 0)

# Check if we have observations in each group
cat("\nGroup sizes:\n")
cat("  Unburned + Wet:", length(unburned_wet), "\n")
cat("  Unburned + Dry:", length(unburned_dry), "\n")
cat("  Burned + Wet:", length(burned_wet), "\n")
cat("  Burned + Dry:", length(burned_dry), "\n")

# Mean for each burn status (averaging over wet/dry)
# stream_quality_samples is [iterations x observations]
if (length(c(unburned_wet, unburned_dry)) > 0) {
  mean_unburned_samples <- rowMeans(stream_quality_samples[,
    c(unburned_wet, unburned_dry), drop = FALSE])
} else {
  mean_unburned_samples <- rep(0, nrow(stream_quality_samples))
}

if (length(c(burned_wet, burned_dry)) > 0) {
  mean_burned_samples <- rowMeans(stream_quality_samples[,
    c(burned_wet, burned_dry), drop = FALSE])
} else {
  mean_burned_samples <- rep(0, nrow(stream_quality_samples))
}

# Mean for each wet/dry status (averaging over burn)
if (length(c(unburned_dry, burned_dry)) > 0) {
  mean_dry_samples <- rowMeans(stream_quality_samples[,
    c(unburned_dry, burned_dry), drop = FALSE])
} else {
  mean_dry_samples <- rep(0, nrow(stream_quality_samples))
}

if (length(c(unburned_wet, burned_wet)) > 0) {
  mean_wet_samples <- rowMeans(stream_quality_samples[,
    c(unburned_wet, burned_wet), drop = FALSE])
} else {
  mean_wet_samples <- rep(0, nrow(stream_quality_samples))
}

# Calculate summaries
mean_unburned <- list(
  median = median(mean_unburned_samples),
  lower = quantile(mean_unburned_samples, 0.025),
  upper = quantile(mean_unburned_samples, 0.975)
)

mean_burned <- list(
  median = median(mean_burned_samples),
  lower = quantile(mean_burned_samples, 0.025),
  upper = quantile(mean_burned_samples, 0.975)
)

mean_dry <- list(
  median = median(mean_dry_samples),
  lower = quantile(mean_dry_samples, 0.025),
  upper = quantile(mean_dry_samples, 0.975)
)

mean_wet <- list(
  median = median(mean_wet_samples),
  lower = quantile(mean_wet_samples, 0.025),
  upper = quantile(mean_wet_samples, 0.975)
)

# Trout effects
beta_trout_presence <- extract_param("beta_trout_presence")
beta_trout_abundance <- extract_param("beta_trout_abundance")

#--------------------------------
## Define DAG structure
#--------------------------------

# Layout: Predictors (left) -> Latent Variable (center) -> Indicators (above/below in lines) + Outcomes (right)
dag <- dagitty('dag {
  BurnStatus [exposure,pos="0,2"]
  WetDry [exposure,pos="0,3"]

  StreamQuality [latent,pos="3,2.5"]

  Conductivity [outcome,pos="2,4"]
  Depth [outcome,pos="3,4"]
  DO [outcome,pos="4,4"]
  Thermal [outcome,pos="2,1"]
  Canopy [outcome,pos="3,1"]
  Q [outcome,pos="4,1"]

  TroutPresence [outcome,pos="6,2"]
  TroutAbundance [outcome,pos="6,3"]

  BurnStatus -> StreamQuality
  WetDry -> StreamQuality

  StreamQuality -> Conductivity
  StreamQuality -> Depth
  StreamQuality -> DO
  StreamQuality -> Thermal
  StreamQuality -> Canopy
  StreamQuality -> Q

  StreamQuality -> TroutPresence
  StreamQuality -> TroutAbundance
}')

tidy_dag <- tidy_dagitty(dag)

#--------------------------------
## Version 1: DAG without parameter estimates
#--------------------------------

p1 <- ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(edge_width = 0.8, arrow_directed = grid::arrow(length = grid::unit(8, "pt"), type = "closed")) +
  geom_dag_point(aes(color = name == "StreamQuality"), size = 16) +
  geom_dag_text(color = "white", size = 3.5, fontface = "bold") +
  scale_color_manual(values = c("grey30", "steelblue"), guide = "none") +
  theme_dag() +
  labs(title = "Latent Variable SEM: Stream Quality Model",
       subtitle = "Predictors → Latent Stream Quality → Environmental Indicators & Trout Outcomes")

print(p1)
ggsave("Figures/sem_dag_clean.png", p1, width = 12, height = 10, dpi = 300)

#--------------------------------
## Version 2: DAG with parameter estimates
#--------------------------------

# Create edge labels with parameter estimates
# For categorical predictors: reference level (intercept) = 0, other level = beta
# This matches standard regression output
unburned_effect <- list(median = 0, lower = 0, upper = 0)  # Reference
burned_effect <- beta_burned  # Deviation from reference

dry_effect <- list(median = 0, lower = 0, upper = 0)  # Reference
wet_effect <- beta_wet  # Deviation from reference

edge_labels <- data.frame(
  x = c(1.5, 1.5, 2.3, 3, 3.7, 2.3, 3, 3.7, 4.5, 4.5),
  y = c(2.25, 2.75, 3.4, 3.4, 3.4, 1.6, 1.6, 1.6, 2.25, 2.75),
  label = c(
    sprintf("Unburned: 0 (ref)\nBurned: %.2f [%.2f, %.2f]",
            burned_effect$median, burned_effect$lower, burned_effect$upper),
    sprintf("Dry: 0 (ref)\nWet: %.2f [%.2f, %.2f]",
            wet_effect$median, wet_effect$lower, wet_effect$upper),
    sprintf("%.2f\n[%.2f, %.2f]", beta_conduct$median,
            beta_conduct$lower, beta_conduct$upper),
    sprintf("%.2f\n[%.2f, %.2f]", beta_depth$median,
            beta_depth$lower, beta_depth$upper),
    sprintf("%.2f\n[%.2f, %.2f]", beta_do$median,
            beta_do$lower, beta_do$upper),
    sprintf("%.2f\n[%.2f, %.2f]", beta_thermal$median,
            beta_thermal$lower, beta_thermal$upper),
    sprintf("%.2f\n[%.2f, %.2f]", beta_canopy$median,
            beta_canopy$lower, beta_canopy$upper),
    sprintf("%.2f\n[%.2f, %.2f]", beta_q$median,
            beta_q$lower, beta_q$upper),
    sprintf("%.2f\n[%.2f, %.2f]", beta_trout_presence$median,
            beta_trout_presence$lower, beta_trout_presence$upper),
    sprintf("%.2f\n[%.2f, %.2f]", beta_trout_abundance$median,
            beta_trout_abundance$lower, beta_trout_abundance$upper)
  )
)

p2 <- ggplot(tidy_dag, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(edge_width = 0.8, arrow_directed = grid::arrow(length = grid::unit(8, "pt"), type = "closed")) +
  geom_dag_point(aes(color = name == "StreamQuality"), size = 16) +
  geom_dag_text(color = "white", size = 3.5, fontface = "bold") +
  geom_label(data = edge_labels, aes(x = x, y = y, label = label),
            size = 2.5, fill = "white", alpha = 0.9,
            label.padding = unit(0.15, "lines"),
            inherit.aes = FALSE) +
  scale_color_manual(values = c("grey30", "steelblue"), guide = "none") +
  theme_dag() +
  labs(title = "Latent Variable SEM with Parameter Estimates",
       subtitle = "Median [95% Credible Interval] for each path coefficient")

print(p2)
ggsave("Figures/sem_dag_with_estimates.png", p2, width = 12, height = 10, dpi = 300)

#--------------------------------
## Print parameter summary
#--------------------------------

cat("\n=== PATH COEFFICIENTS ===\n\n")

cat("PREDICTORS -> STREAM QUALITY:\n")
cat("  Burn Status Effects (relative to Unburned baseline):\n")
cat(sprintf("    Unburned: %.2f [%.2f, %.2f]\n",
            mean_unburned$median, mean_unburned$lower, mean_unburned$upper))
cat(sprintf("    Burned: %.2f [%.2f, %.2f]\n",
            mean_burned$median, mean_burned$lower, mean_burned$upper))
cat("  Wet/Dry Effects (relative to Dry baseline):\n")
cat(sprintf("    Dry: %.2f [%.2f, %.2f]\n",
            mean_dry$median, mean_dry$lower, mean_dry$upper))
cat(sprintf("    Wet: %.2f [%.2f, %.2f]\n\n",
            mean_wet$median, mean_wet$lower, mean_wet$upper))

cat("STREAM QUALITY -> INDICATORS:\n")
cat(sprintf("  StreamQuality -> Conductivity: %.2f [%.2f, %.2f]\n",
            beta_conduct$median, beta_conduct$lower, beta_conduct$upper))
cat(sprintf("  StreamQuality -> Depth: %.2f [%.2f, %.2f]\n",
            beta_depth$median, beta_depth$lower, beta_depth$upper))
cat(sprintf("  StreamQuality -> DO: %.2f [%.2f, %.2f]\n",
            beta_do$median, beta_do$lower, beta_do$upper))
cat(sprintf("  StreamQuality -> Thermal: %.2f [%.2f, %.2f]\n",
            beta_thermal$median, beta_thermal$lower, beta_thermal$upper))
cat(sprintf("  StreamQuality -> Canopy: %.2f [%.2f, %.2f]\n",
            beta_canopy$median, beta_canopy$lower, beta_canopy$upper))
cat(sprintf("  StreamQuality -> Q: %.2f [%.2f, %.2f]\n\n",
            beta_q$median, beta_q$lower, beta_q$upper))

cat("STREAM QUALITY -> TROUT:\n")
cat(sprintf("  StreamQuality -> TroutPresence: %.2f [%.2f, %.2f]\n",
            beta_trout_presence$median, beta_trout_presence$lower, beta_trout_presence$upper))
cat(sprintf("  StreamQuality -> TroutAbundance: %.2f [%.2f, %.2f]\n\n",
            beta_trout_abundance$median, beta_trout_abundance$lower, beta_trout_abundance$upper))

cat("\nDAG visualizations saved:\n")
cat("  - Figures/sem_dag_clean.png (without estimates)\n")
cat("  - Figures/sem_dag_with_estimates.png (with median and 95% CI)\n")

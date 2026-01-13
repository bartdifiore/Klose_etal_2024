#--------------------------------
## Compare Stan Latent Variable Models using LOO-CV
## Compares: Full, Full-NoPred, Full-Interaction, Reduced models
## Creates intuitive comparison tables and visualizations
#--------------------------------

library(tidyverse)
library(rstan)
library(loo)
library(knitr)

#--------------------------------
## Load fitted models
#--------------------------------

cat("Loading fitted models...\n")

models_to_load <- list(
  full = "Models/stan_model_full_fit.rds",
  full_nopred = "Models/stan_model_full_nopred_fit.rds",
  full_interaction = "Models/stan_model_full_interaction_fit.rds",
  reduced = "Models/stan_model_reduced_fit.rds"
)

# Check which models exist
models_exist <- sapply(models_to_load, file.exists)
cat("\nModel availability:\n")
print(data.frame(Model = names(models_to_load), Available = models_exist))

# Load available models
models <- list()
for (model_name in names(models_to_load)) {
  if (models_exist[model_name]) {
    cat("Loading", model_name, "model...\n")
    models[[model_name]] <- readRDS(models_to_load[[model_name]])
  }
}

if (length(models) == 0) {
  stop("No fitted models found! Please fit models first.")
}

cat("\nSuccessfully loaded", length(models), "models.\n")

#--------------------------------
## Extract log-likelihood and compute LOO
#--------------------------------

cat("\nComputing LOO-CV for each model...\n")

loo_results <- list()
pareto_k_counts <- list()

for (model_name in names(models)) {
  cat("Processing", model_name, "...\n")

  # Extract log-likelihood
  log_lik <- extract_log_lik(models[[model_name]], parameter_name = "log_lik")

  # Compute LOO
  loo_results[[model_name]] <- loo(log_lik, cores = 4)

  # Extract Pareto k diagnostics
  pareto_k <- loo_results[[model_name]]$diagnostics$pareto_k
  pareto_k_counts[[model_name]] <- data.frame(
    Model = model_name,
    Good = sum(pareto_k < 0.5),
    OK = sum(pareto_k >= 0.5 & pareto_k < 0.7),
    Bad = sum(pareto_k >= 0.7 & pareto_k < 1.0),
    VeryBad = sum(pareto_k >= 1.0),
    Max_k = max(pareto_k)
  )
}

#--------------------------------
## Table 1: Model Specifications
#--------------------------------

cat("\n" , rep("=", 60), "\n", sep = "")
cat("TABLE 1: MODEL SPECIFICATIONS\n")
cat(rep("=", 60), "\n", sep = "")

model_specs <- data.frame(
  Model = c("Full", "Full-NoPred", "Full-Interaction", "Reduced"),
  Description = c(
    "6 indicators + burn + wet (additive)",
    "6 indicators + NO predictors",
    "6 indicators + burn + wet + interaction",
    "4 indicators + burn + wet (additive)"
  ),
  N_Indicators = c(6, 6, 6, 4),
  Predictors = c("burn, wet", "none", "burn, wet, burn×wet", "burn, wet"),
  N_Predictor_Params = c(3, 1, 4, 3),  # alpha + betas
  stringsAsFactors = FALSE
)

# Filter to available models
model_specs <- model_specs %>%
  filter(tolower(gsub("-", "_", Model)) %in% names(models))

print(kable(model_specs, align = "l"))

#--------------------------------
## Table 2: LOO-CV Model Comparison
#--------------------------------

cat("\n", rep("=", 60), "\n", sep = "")
cat("TABLE 2: LOO-CV MODEL COMPARISON\n")
cat(rep("=", 60), "\n", sep = "")

# Extract key LOO statistics
loo_table <- data.frame(
  Model = names(loo_results),
  ELPD_loo = sapply(loo_results, function(x) x$estimates["elpd_loo", "Estimate"]),
  SE_elpd = sapply(loo_results, function(x) x$estimates["elpd_loo", "SE"]),
  P_loo = sapply(loo_results, function(x) x$estimates["p_loo", "Estimate"]),
  SE_p = sapply(loo_results, function(x) x$estimates["p_loo", "SE"]),
  LOOIC = sapply(loo_results, function(x) x$estimates["looic", "Estimate"]),
  SE_looic = sapply(loo_results, function(x) x$estimates["looic", "SE"])
) %>%
  arrange(desc(ELPD_loo))  # Best model first

# Add model complexity ratio (n/p_loo)
loo_table <- loo_table %>%
  mutate(
    N = sapply(models[Model], function(x) dim(extract(x, "log_lik")$log_lik)[2]),
    n_over_p = N / P_loo
  )

print(kable(loo_table, digits = 2, align = "l"))

cat("\nInterpretation:\n")
cat("- ELPD_loo: Expected log pointwise predictive density (HIGHER is better)\n")
cat("- P_loo: Effective number of parameters (lower = more parsimonious)\n")
cat("- LOOIC: LOO Information Criterion (LOWER is better, = -2*ELPD_loo)\n")
cat("- n/p_loo: Sample size / effective parameters (should be >5, ideally >10)\n")

#--------------------------------
## Table 3: Pairwise Model Comparisons
#--------------------------------

if (length(loo_results) > 1) {
  cat("\n", rep("=", 60), "\n", sep = "")
  cat("TABLE 3: PAIRWISE MODEL COMPARISONS\n")
  cat(rep("=", 60), "\n", sep = "")

  # Compare all models pairwise
  model_names <- names(loo_results)
  comparison_results <- list()

  for (i in 1:(length(model_names) - 1)) {
    for (j in (i + 1):length(model_names)) {
      model1 <- model_names[i]
      model2 <- model_names[j]

      comp <- loo_compare(loo_results[[model1]], loo_results[[model2]])

      # Extract the difference
      elpd_diff <- comp[2, "elpd_diff"]
      se_diff <- comp[2, "se_diff"]

      comparison_results[[paste(model1, "vs", model2)]] <- data.frame(
        Model1 = model1,
        Model2 = model2,
        ELPD_diff = elpd_diff,
        SE_diff = se_diff,
        Z_score = elpd_diff / se_diff,
        Interpretation = ifelse(
          abs(elpd_diff) < 2 * se_diff,
          "No clear difference",
          ifelse(elpd_diff > 0, paste(model1, "better"), paste(model2, "better"))
        )
      )
    }
  }

  comparison_table <- do.call(rbind, comparison_results) %>%
    arrange(desc(abs(ELPD_diff)))

  print(kable(comparison_table, digits = 2, align = "l", row.names = FALSE))

  cat("\nInterpretation:\n")
  cat("- ELPD_diff: Difference in expected log predictive density\n")
  cat("- SE_diff: Standard error of the difference\n")
  cat("- Rule of thumb: |ELPD_diff| > 2*SE suggests meaningful difference\n")
  cat("- Positive ELPD_diff: Model1 is better; Negative: Model2 is better\n")
}

#--------------------------------
## Table 4: Pareto k Diagnostics
#--------------------------------

cat("\n", rep("=", 60), "\n", sep = "")
cat("TABLE 4: PARETO K DIAGNOSTICS\n")
cat(rep("=", 60), "\n", sep = "")

pareto_table <- do.call(rbind, pareto_k_counts) %>%
  arrange(desc(VeryBad), desc(Bad))

print(kable(pareto_table, digits = 3, align = "l", row.names = FALSE))

cat("\nInterpretation:\n")
cat("- Pareto k < 0.5: Good (reliable LOO approximation)\n")
cat("- Pareto k 0.5-0.7: OK (acceptable)\n")
cat("- Pareto k 0.7-1.0: Bad (influential observation, LOO less reliable)\n")
cat("- Pareto k > 1.0: Very bad (highly influential, LOO unreliable)\n")
cat("- Ideally: all observations should have k < 0.7\n")

#--------------------------------
## Table 5: Best Model Summary
#--------------------------------

cat("\n", rep("=", 60), "\n", sep = "")
cat("TABLE 5: BEST MODEL PARAMETER ESTIMATES\n")
cat(rep("=", 60), "\n", sep = "")

# Identify best model (highest ELPD)
best_model_name <- loo_table$Model[1]
best_model <- models[[best_model_name]]

cat("Best model by LOO-CV:", best_model_name, "\n\n")

# Extract key parameter estimates
summary_best <- summary(best_model)$summary

# Parameters of interest
params_of_interest <- c(
  "alpha_quality", "beta_burned", "beta_wet", "beta_interaction",
  "beta_conduct", "beta_depth", "beta_do", "beta_thermal", "beta_canopy", "beta_q",
  "beta_trout", "alpha_trout"
)

# Filter to parameters that exist in this model
params_exist <- params_of_interest[params_of_interest %in% rownames(summary_best)]

param_table <- as.data.frame(summary_best[params_exist, c("mean", "sd", "2.5%", "97.5%", "Rhat", "n_eff")]) %>%
  rownames_to_column("Parameter") %>%
  mutate(
    Significant = ifelse(sign(`2.5%`) == sign(`97.5%`), "Yes", "No")
  )

print(kable(param_table, digits = 3, align = "l"))

cat("\nInterpretation:\n")
cat("- 95% CI excludes zero → 'Significant' = Yes\n")
cat("- Rhat should be < 1.01 for convergence\n")
cat("- n_eff: effective sample size (higher is better)\n")

#--------------------------------
## Visual Comparison: LOO-CV
#--------------------------------

cat("\n", rep("=", 60), "\n", sep = "")
cat("GENERATING COMPARISON PLOTS\n")
cat(rep("=", 60), "\n")

# Create comparison plot
if (length(loo_results) > 1) {
  pdf("Figures/model_comparison_loo.pdf", width = 10, height = 6)

  # Plot 1: ELPD comparison
  p1 <- ggplot(loo_table, aes(x = reorder(Model, ELPD_loo), y = ELPD_loo)) +
    geom_point(size = 4) +
    geom_errorbar(aes(ymin = ELPD_loo - 2*SE_elpd, ymax = ELPD_loo + 2*SE_elpd), width = 0.2) +
    coord_flip() +
    labs(
      title = "Model Comparison: Expected Log Predictive Density",
      subtitle = "Higher is better; error bars show ±2 SE",
      x = "Model",
      y = "ELPD (LOO-CV)"
    ) +
    theme_minimal(base_size = 12)

  print(p1)

  # Plot 2: Model complexity (n/p_loo ratio)
  p2 <- ggplot(loo_table, aes(x = reorder(Model, n_over_p), y = n_over_p)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_hline(yintercept = 5, linetype = "dashed", color = "red", linewidth = 1) +
    geom_text(aes(label = round(n_over_p, 2)), vjust = -0.5) +
    coord_flip() +
    labs(
      title = "Model Complexity: Sample Size / Effective Parameters",
      subtitle = "Red line at 5 (minimum acceptable); higher is better",
      x = "Model",
      y = "n / p_loo"
    ) +
    theme_minimal(base_size = 12)

  print(p2)

  # Plot 3: Pareto k diagnostics by model
  pareto_long <- pareto_table %>%
    pivot_longer(cols = c(Good, OK, Bad, VeryBad), names_to = "Category", values_to = "Count") %>%
    mutate(Category = factor(Category, levels = c("Good", "OK", "Bad", "VeryBad")))

  p3 <- ggplot(pareto_long, aes(x = Model, y = Count, fill = Category)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(
      values = c("Good" = "darkgreen", "OK" = "yellow", "Bad" = "orange", "VeryBad" = "red"),
      labels = c("Good (k<0.5)", "OK (0.5-0.7)", "Bad (0.7-1.0)", "Very Bad (k>1.0)")
    ) +
    coord_flip() +
    labs(
      title = "Pareto k Diagnostics by Model",
      subtitle = "Distribution of observation influence on LOO-CV",
      x = "Model",
      y = "Number of Observations",
      fill = "Pareto k Category"
    ) +
    theme_minimal(base_size = 12)

  print(p3)

  dev.off()

  cat("\nPlots saved to: Figures/model_comparison_loo.pdf\n")
}

#--------------------------------
## Summary and Recommendations
#--------------------------------

cat("\n", rep("=", 80), "\n", sep = "")
cat("SUMMARY AND RECOMMENDATIONS\n")
cat(rep("=", 80), "\n", sep = "")

cat("\nBest model (by ELPD):", best_model_name, "\n")
cat("  - ELPD (LOO):", round(loo_table$ELPD_loo[1], 2), "±", round(loo_table$SE_elpd[1], 2), "\n")
cat("  - Effective parameters:", round(loo_table$P_loo[1], 2), "\n")
cat("  - n/p_loo ratio:", round(loo_table$n_over_p[1], 2), "\n")

if (loo_table$n_over_p[1] < 5) {
  cat("\n⚠️  WARNING: Best model has n/p_loo < 5, indicating potential overfitting!\n")
  cat("    Consider: (1) Collecting more data, (2) Using a simpler model, or (3) Stronger priors\n")
}

if (length(loo_results) > 1) {
  # Check if second-best model is competitive
  elpd_diff <- loo_table$ELPD_loo[1] - loo_table$ELPD_loo[2]
  se_diff_approx <- sqrt(loo_table$SE_elpd[1]^2 + loo_table$SE_elpd[2]^2)

  if (abs(elpd_diff) < 2 * se_diff_approx) {
    cat("\n📊 Second-best model (", loo_table$Model[2], ") is competitive (within 2 SE).\n", sep = "")
    cat("    Consider using the simpler model if it has fewer parameters.\n")
  } else {
    cat("\n✓ Best model is clearly superior (ELPD difference >2 SE).\n")
  }
}

# Check for interaction effect if interaction model was fit
if ("full_interaction" %in% names(models)) {
  interaction_summary <- summary(models[["full_interaction"]])$summary
  if ("beta_interaction" %in% rownames(interaction_summary)) {
    beta_int <- interaction_summary["beta_interaction", ]
    ci_low <- beta_int["2.5%"]
    ci_high <- beta_int["97.5%"]

    cat("\n🔍 Interaction effect (burn × wet):\n")
    cat("    Estimate:", round(beta_int["mean"], 3),
        " [", round(ci_low, 3), ", ", round(ci_high, 3), "]\n", sep = "")

    if (sign(ci_low) == sign(ci_high)) {
      cat("    ✓ 95% CI excludes zero: Interaction is credible\n")
    } else {
      cat("    ✗ 95% CI includes zero: No clear evidence for interaction\n")
    }
  }
}

cat("\n", rep("=", 80), "\n", sep = "")
cat("Analysis complete! See tables and plots above for details.\n")
cat(rep("=", 80), "\n", sep = "")



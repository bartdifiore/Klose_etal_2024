#--------------------------------
## Run Brown's Reference Model and Compute LOO Statistics
## To compare with our stream quality models
#--------------------------------

library(tidyverse)
library(rstan)
library(loo)

# Set working directory to reference model repo
setwd("/Users/bart/Github/ecological-condition-latent-model")

#--------------------------------
## Prepare data (copied from fit-indicator-model.R)
#--------------------------------

dat <- read.csv("Data for Barramundi model.csv")
dat$catch <- dat$Barramundicatch
dat$days <- dat$Effort
n <- nrow(dat)

s2 <- function(x){
  (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
}

# Prep data
catchability_increase <- 1

dat2 <- dat %>%
  mutate(Streamflow = log(Streamflow_wetseason)) %>%
  filter(!is.na(Streamflow)) %>%
  mutate(days_std = (days/365)*catchability_increase^(Year-1990),
         ln_cpue = log(catch/days_std),
         Streamflow_std = (Streamflow - mean(Streamflow, na.rm = TRUE))/sd(Streamflow, na.rm = TRUE),
         ndvi_std = s2(NDVI),
         pasture_std = s2(pasture_bio)
         )

cat("Sample size:", nrow(dat2), "\n")

#--------------------------------
## Prior params (from original script)
#--------------------------------

logNormalParams <- function(mB, sigma_B){
  a <-2*log(mB) - 0.5 * log(sigma_B^2 + mB^2)
  b <- sqrt(-2*log(mB)+log(sigma_B^2 + mB^2))
  return(list(lmean = a, lsigma = b))
}

r_est <- 0.3
bf_guess <- 0.2
b1_fract <- 10*(bf_guess/0.2)
B0_CV <- 1

B1_guess <- dat2$catch[1]*b1_fract
B0_guess <- B1_guess*(1/bf_guess)
B0params <- logNormalParams(B0_guess, B0_guess * B0_CV)
rparams <- logNormalParams(r_est, r_est*0.3)

#--------------------------------
## Setup Stan data
#--------------------------------

datstan <- with(dat2, {
  x = list(N = nrow(dat2),
       flow = Streamflow_std,
       lnCPUE = ln_cpue,
       catches = catch,
       logK_mean = B0params$lmean,
       logK_sd = B0params$lsigma,
       init_fraction = bf_guess,
       logr_mean = rparams$lmean,
       logr_sd = rparams$lsigma,
       Nndvi = sum(!is.na(ndvi_std)),
       Npasture = sum(!is.na(pasture_std)),
       i_ndvi = which(!is.na(ndvi_std)),
       i_pasture = which(!is.na(pasture_std))
  )
  x$ndvi = ndvi_std[x$i_ndvi]
  x$pasture = pasture_std[x$i_pasture]
  x
})

cat("\nData summary:\n")
cat("  N (time points):", datstan$N, "\n")
cat("  N NDVI observations:", datstan$Nndvi, "\n")
cat("  N Pasture observations:", datstan$Npasture, "\n")

#--------------------------------
## Fit Stan model
#--------------------------------

options(mc.cores = 4)

cat("\nFitting reference model...\n")

fitm1 <- stan(file = "indicator-model.stan",
              data = datstan,
              iter = 5000,
              chains = 4,  # Using 4 chains for consistency
              thin = 5,
              init = list(
                list(lnr = log(r_est), lnK = log(3000), q = 0.05,
                     sigma_cpue = 0.1, sigma_u = 0.05,
                     beta_u = 0.05, beta_nu = 1),
                list(lnr = log(r_est)*1.2, lnK = log(5000), q = 0.1,
                     sigma_cpue = 0.15, sigma_u = 0.1,
                     beta_u = 0.1, beta_nu = 2),
                list(lnr = log(r_est)*0.6, lnK = log(2000), q = 0.02,
                     sigma_cpue = 0.05, sigma_u = 0.2,
                     beta_u = 0.15, beta_nu = 0.1),
                list(lnr = log(r_est)*0.8, lnK = log(4000), q = 0.075,
                     sigma_cpue = 0.12, sigma_u = 0.075,
                     beta_u = 0.08, beta_nu = 1.5)
              ),
              control = list(max_treedepth = 12, adapt_delta = 0.8)
)

cat("\nModel fitting complete!\n")

#--------------------------------
## Convergence diagnostics
#--------------------------------

cat("\n", rep("=", 60), "\n", sep = "")
cat("CONVERGENCE DIAGNOSTICS\n")
cat(rep("=", 60), "\n", sep = "")

# Check divergences
sampler_params <- get_sampler_params(fitm1, inc_warmup = FALSE)
divergences <- sum(sapply(sampler_params, function(x) sum(x[, "divergent__"])))
cat("Divergent transitions:", divergences, "\n")

# Check Rhat
summary_fit <- summary(fitm1)$summary
rhats <- summary_fit[, "Rhat"]
cat("Max Rhat:", max(rhats, na.rm = TRUE), "\n")
cat("Parameters with Rhat > 1.01:", sum(rhats > 1.01, na.rm = TRUE), "\n")

# Check ESS
ess_bulk <- summary_fit[, "n_eff"]
cat("Min ESS (bulk):", min(ess_bulk, na.rm = TRUE), "\n")

#--------------------------------
## Compute LOO-CV
#--------------------------------

cat("\n", rep("=", 60), "\n", sep = "")
cat("LOO-CV ANALYSIS\n")
cat(rep("=", 60), "\n", sep = "")

# Extract log-likelihood arrays
ll_cpue <- extract(fitm1, "ll_cpue")$ll_cpue
ll_ndvi <- extract(fitm1, "ll_ndvi")$ll_ndvi
ll_pasture <- extract(fitm1, "ll_pasture")$ll_pasture

cat("\nLog-likelihood dimensions:\n")
cat("  ll_cpue:", dim(ll_cpue), "\n")
cat("  ll_ndvi:", dim(ll_ndvi), "\n")
cat("  ll_pasture:", dim(ll_pasture), "\n")

# Compute LOO for each component separately
cat("\nComputing LOO for CPUE observations...\n")
loo_cpue <- loo(ll_cpue, cores = 4)

cat("\nComputing LOO for NDVI observations...\n")
loo_ndvi <- loo(ll_ndvi, cores = 4)

cat("\nComputing LOO for Pasture observations...\n")
loo_pasture <- loo(ll_pasture, cores = 4)

# Combine log-likelihoods for overall model LOO
# Need to combine them properly - pad with zeros where observations are missing
n_iter <- dim(ll_cpue)[1]
N <- datstan$N

# Create full log-likelihood matrix
log_lik_full <- matrix(0, nrow = n_iter, ncol = N)

# Add CPUE (all N observations)
log_lik_full <- log_lik_full + ll_cpue

# Add NDVI (only at specific indices)
for (i in 1:length(datstan$i_ndvi)) {
  idx <- datstan$i_ndvi[i]
  log_lik_full[, idx] <- log_lik_full[, idx] + ll_ndvi[, i]
}

# Add Pasture (only at specific indices)
for (i in 1:length(datstan$i_pasture)) {
  idx <- datstan$i_pasture[i]
  log_lik_full[, idx] <- log_lik_full[, idx] + ll_pasture[, i]
}

cat("\nComputing overall LOO (combined observations)...\n")
loo_full <- loo(log_lik_full, cores = 4)

#--------------------------------
## Print LOO results
#--------------------------------

cat("\n", rep("=", 60), "\n", sep = "")
cat("LOO STATISTICS SUMMARY\n")
cat(rep("=", 60), "\n", sep = "")

print(loo_full)

cat("\n\nKey Statistics:\n")
cat("  N observations:", N, "\n")
cat("  ELPD (LOO):", round(loo_full$estimates["elpd_loo", "Estimate"], 2), "±",
    round(loo_full$estimates["elpd_loo", "SE"], 2), "\n")
cat("  P_loo (effective parameters):", round(loo_full$estimates["p_loo", "Estimate"], 2), "±",
    round(loo_full$estimates["p_loo", "SE"], 2), "\n")
cat("  LOOIC:", round(loo_full$estimates["looic", "Estimate"], 2), "\n")
cat("  n/p_loo ratio:", round(N / loo_full$estimates["p_loo", "Estimate"], 2), "\n")

cat("\n\nPareto k diagnostics:\n")
pareto_k <- loo_full$diagnostics$pareto_k
cat("  Good (k < 0.5):", sum(pareto_k < 0.5), "\n")
cat("  OK (0.5 <= k < 0.7):", sum(pareto_k >= 0.5 & pareto_k < 0.7), "\n")
cat("  Bad (0.7 <= k < 1.0):", sum(pareto_k >= 0.7 & pareto_k < 1.0), "\n")
cat("  Very bad (k >= 1.0):", sum(pareto_k >= 1.0), "\n")
cat("  Max k:", round(max(pareto_k), 3), "\n")

#--------------------------------
## Compare to your models
#--------------------------------

cat("\n", rep("=", 60), "\n", sep = "")
cat("COMPARISON TO STREAM QUALITY MODELS\n")
cat(rep("=", 60), "\n", sep = "")

cat("\nReference model (Barramundi):\n")
cat("  N =", N, "\n")
cat("  p_loo =", round(loo_full$estimates["p_loo", "Estimate"], 2), "\n")
cat("  n/p_loo =", round(N / loo_full$estimates["p_loo", "Estimate"], 2), "\n")

cat("\nYour models (Stream Quality) - from previous output:\n")
cat("  N = 35\n")
cat("  Full model: p_loo = 41.98, n/p_loo = 0.83\n")
cat("  Full-interaction: p_loo = 40.50, n/p_loo = 0.86\n")
cat("  Reduced: p_loo = 56.69, n/p_loo = 0.62\n")
cat("  Full-nopred: p_loo = 60.45, n/p_loo = 0.58\n")

# Count actual parameters in reference model
cat("\n\nActual parameter counts:\n")
cat("Reference model parameters:\n")
cat("  - nu_raw[N]: ", N, " parameters\n", sep = "")
cat("  - Stock model: 5 params (lnK, lnr, q, beta_u, sigma_u)\n")
cat("  - CPUE observation: 1 param (sigma_cpue)\n")
cat("  - Indicators: 6 params (2 betas + 2 alphas + 2 sigmas)\n")
cat("  - Latent predictor: 1 param (beta_nu)\n")
cat("  - TOTAL (excluding nu_raw): ~13 params\n")
cat("  - TOTAL (including nu_raw): ~", N + 13, " params\n", sep = "")

cat("\n", rep("=", 60), "\n", sep = "")
cat("Analysis complete!\n")
cat(rep("=", 60), "\n", sep = "")

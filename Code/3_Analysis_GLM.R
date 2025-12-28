#--------------------------------
## Option 1: Simple GLM Analysis
## Analyzing trout presence/absence using logistic regression
#--------------------------------

# Load libraries
library(tidyverse)
library(DHARMa)
library(GGally)
library(car)
library(ggeffects)

# Load data
df <- read.csv("Data/cleaned_20240408.csv")

# Prepare data for modeling
df_mod <- df %>%
  rename(b_u = burned_b_vs_unburned_u,
         max_depth = max_depth_m,
         avg_canopy = average_canopy_cover,
         conduct = conductivity_u_s_cm,
         do = point_minimum_do_mg_l,
         q = q_estimate_m3_s,
         avg_chlorophyll = average_chlorophyll_a_mg_m2) %>%
  mutate(trout = as.integer(ifelse(trout_present_absent == "P", 1, 0)),
         b_u = as.factor(b_u))

#--------------------------------
## Check for collinearity
#--------------------------------

# Visualize correlations between predictors
df_mod %>%
  select(max_depth:pct_cover, total_inverts, thermal_index) %>%
  ggpairs()

#--------------------------------
## Model 1: Full model with pct_cover
#--------------------------------

mod1 <- glm(trout ~ max_depth + q + avg_canopy + conduct + do + pct_cover +
              thermal_index + avg_chlorophyll + total_inverts,
            data = df_mod, family = "binomial")

summary(mod1)

# Check variance inflation factors (VIF < 5 is desired)
data.frame(vif = vif(mod1))

#--------------------------------
## Model 2: Transformed predictors with pct_cover
#--------------------------------

df_mod2 <- df_mod %>%
  mutate(q_log = log(q),
         avg_canopy_logit = qlogis(avg_canopy/100),
         conduct_log = log(conduct))

mod2 <- glm(trout ~ scale(max_depth) + scale(q_log) + scale(avg_canopy_logit) +
              scale(conduct_log) + scale(do) + scale(pct_cover) +
              scale(thermal_index) + scale(avg_chlorophyll) + scale(total_inverts),
            data = df_mod2, family = "binomial")

# Check VIF
data.frame(vif = vif(mod2))

#--------------------------------
## Model 3: Simplified model (conductivity only)
#--------------------------------

mod3 <- glm(trout ~ scale(conduct_log) + scale(avg_chlorophyll) + scale(total_inverts),
            data = df_mod2, family = "binomial")

summary(mod3)
vif(mod3)

# Plot effect of conductivity
plot(ggpredict(mod3, terms = "conduct_log[all]"))

#--------------------------------
## Models without pct_cover (increases sample size)
#--------------------------------

# Model 4: Full model without pct_cover
mod4 <- glm(trout ~ max_depth + q + avg_canopy + conduct + do +
              thermal_index + avg_chlorophyll + total_inverts,
            data = df_mod, family = "binomial")

summary(mod4)
data.frame(vif = vif(mod4))

# Check residuals
plot(simulateResiduals(mod4))

# Model 5: Transformed predictors without pct_cover (RECOMMENDED)
mod5 <- glm(trout ~ scale(max_depth) + scale(q_log) + scale(avg_canopy_logit) +
              scale(conduct_log) + scale(do) + scale(thermal_index) +
              scale(avg_chlorophyll) + scale(total_inverts),
            data = df_mod2, family = "binomial")

summary(mod5)
data.frame(vif = vif(mod5))

# Check residuals
plot(simulateResiduals(mod5))

#--------------------------------
## Visualize main result
#--------------------------------

# Plot partial regression for conductivity
plot(ggpredict(mod5, terms = "conduct_log[all]"), rawdata = TRUE)

# Check correlations among transformed predictors
df_mod2 %>%
  select(max_depth, q_log, avg_canopy_logit, conduct_log, do,
         thermal_index, avg_chlorophyll, total_inverts) %>%
  ggpairs()

#--------------------------------
## Key findings:
## - Conductivity is the primary predictor of trout presence
## - Higher conductivity decreases probability of trout presence
## - Other variables are not significant predictors (possibly due to collinearity)
#--------------------------------

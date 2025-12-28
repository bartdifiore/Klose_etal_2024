#--------------------------------
## Option 2: Structural Equation Model (SEM) Analysis
## Using piecewise SEM to model cascading relationships
#--------------------------------

# Load libraries
library(tidyverse)
library(piecewiseSEM)
library(dagitty)
library(ggdag)

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

# Transform predictors
df_mod2 <- df_mod %>%
  mutate(q_log = log(q),
         avg_canopy_logit = qlogis(avg_canopy/100),
         conduct_log = log(conduct))

#--------------------------------
## Visualize hypothesized DAG
#--------------------------------

# Create directed acyclic graph (DAG) of hypothesized relationships
dag1 <- dagify(Trout ~ Inverts,
               Inverts ~ Algae,
               Algae ~ Thermal_index,
               Thermal_index ~ Max_depth,
               Max_depth ~ Conductivity)

plot(dag1)

#--------------------------------
## SEM Model 1: Initial hypothesis
#--------------------------------

sem1 <- psem(
  lm(max_depth ~ conduct_log, df_mod2),
  lm(thermal_index ~ max_depth, df_mod2),
  lm(avg_chlorophyll ~ thermal_index, df_mod2),
  lm(total_inverts ~ avg_chlorophyll, df_mod2),
  glm(trout ~ total_inverts, df_mod2, family = "binomial")
)

summary(sem1)
plot(sem1)

# Note: Global goodness-of-fit test suggests model is not a good fit
# Tests of directed separation suggest adding:
#   - thermal_index ~ conductivity
#   - trout ~ conductivity

#--------------------------------
## SEM Model 2: Revised model with additional paths
#--------------------------------

sem2 <- psem(
  lm(max_depth ~ conduct_log, df_mod2),
  lm(thermal_index ~ max_depth + conduct_log, df_mod2),
  lm(avg_chlorophyll ~ thermal_index, df_mod2),
  lm(total_inverts ~ avg_chlorophyll, df_mod2),
  glm(trout ~ total_inverts + conduct_log, df_mod2, family = "binomial")
)

summary(sem2)
plot(sem2)

#--------------------------------
## Key findings:
## - Conductivity is a key predictor of trout presence
## - Little evidence for bottom-up cascade (stream condition -> algae -> inverts -> trout)
## - Conductivity directly influences multiple environmental variables
## - Model 2 provides adequate fit to the data
#--------------------------------

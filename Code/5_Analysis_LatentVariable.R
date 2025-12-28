#--------------------------------
## Option 3: Latent Variable SEM Analysis
## Using lavaan to model stream quality as a latent state
## predicting trout presence
#--------------------------------

# Load libraries
library(tidyverse)
library(lavaan)
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
## Latent variable model specification
#--------------------------------

# Define latent variable model
# "quality" is a latent variable representing stream quality
# It is indicated by 6 environmental variables
# The latent quality state predicts trout presence

lv1 <- '
# latent variable (stream quality)
quality =~ q_log + thermal_index + avg_canopy_logit + conduct_log + do + max_depth

# structural path (quality predicts trout presence)
trout ~ quality
'

# Fit the model
lv1_mod <- sem(lv1, df_mod2, std.lv = TRUE)

# View results
summary(lv1_mod, standardized = TRUE)

#--------------------------------
## Visualize latent variable model
#--------------------------------

# Define coordinates for DAG plot
coords <- list(
  x = c(Trout = 2,
        q_log = 0,
        thermal_index = 0,
        avg_canopy_logit = 0,
        conduct_log = 0,
        do = 0,
        max_depth = 0,
        quality = 1),
  y = c(Trout = 2.5,
        q_log = 5,
        thermal_index = 4,
        avg_canopy_logit = 3,
        conduct_log = 2,
        do = 1,
        max_depth = 0,
        quality = 2.5)
)

# Create DAG
dag1 <- dagify(q_log ~ quality,
               thermal_index ~ quality,
               avg_canopy_logit ~ quality,
               conduct_log ~ quality,
               do ~ quality,
               max_depth ~ quality,
               Trout ~ quality,
               coords = coords)

# Extract standardized estimates for plotting
estimates <- summary(lv1_mod, standardized = TRUE)$pe[1:7, ] %>%
  select(rhs, lhs, std.all) %>%
  mutate(x = ifelse(lhs == "quality", 0.3, 1.5),
         y = c(4.8, 3.75, 3, 2.25, 1.5, 0.5, 2.75),
         std.all = round(std.all, 2))

# Plot DAG with standardized estimates
tidy_dagitty(dag1, layout = "fr") %>%
  mutate(latent = ifelse(name == "quality", "latent", "indicator"),
         size_cat = ifelse(name %in% c("quality", "Trout"), "large", "small"),
         significance = ifelse(to == "do", "dashed", "solid")) %>%
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_node(aes(shape = latent, size = size_cat), show.legend = FALSE, alpha = 0.25) +
  scale_shape_manual(values = c(15, 16)) +
  scale_size_manual(values = c(40, 20)) +
  geom_dag_text(color = "black") +
  geom_dag_edges(aes(edge_linetype = significance), show.legend = FALSE) +
  scale_linetype_manual(values = c(1, 5)) +
  annotate(geom = "text", x = estimates$x, y = estimates$y, label = estimates$std.all) +
  theme_dag()

#--------------------------------
## Key findings:
## - Chi-square test (p >= 0.05) indicates model fits the data
## - All indicators except DO significantly load on the latent "quality" variable
## - Strongest loadings: conductivity (highest), thermal_index, avg_canopy, max_depth, q
## - Latent stream quality significantly predicts trout presence (std. estimate = 0.699)
## - This approach develops a stream quality index for predicting trout presence
##
## Next steps:
## - Consider Bayesian implementation (STAN/brms) for:
##   * Proper binomial distribution for binary trout response
##   * Spatial predictions with confidence intervals
##   * More flexible model specification
#--------------------------------

library(tidyverse)


df <- read.csv("Data/cleaned_20240408.csv")


# Look for collinearity of predictors

df %>% 
  select(max_depth_m:pct_cover) %>% 
  GGally::ggpairs() # Everything of interest is collinear...

df %>% as_tibble() %>%
  mutate(avg_canopy_logit = qlogis(average_canopy_cover/100), 
         conduct_log = log(conductivity_u_s_cm)) %>%
  select(max_depth_m, point_minimum_do_mg_l, avg_canopy_logit, conduct_log) %>% 
  GGally::ggpairs()
  
hist(df$total_trout, breaks = c(-1, 0, 10, 20, 30, max(df$total_trout, na.rm = T)+1))

ggplot(df, aes(x = trout_present_absent))+
  geom_bar()

df %>%
  group_by(year) %>% 
  summarize(n = n())

# Patterns w/ burn status or drought? 



ggplot(df, aes(x = wet_or_dry_in_2016_1, y = trout_present_absent))+
  geom_jitter(width = 0.1, height = 0.1)

ggplot(df, aes(x = burned_b_vs_unburned_u, y = trout_present_absent))+
  geom_jitter(width = 0.1, height = 0.1)


ggplot(df, aes(x = trout_present_absent, y = average_canopy_cover))+
  geom_point()


df_mod <- df %>%
  mutate(trout = ifelse(trout_present_absent == "P", 1, 0))

mod1 <- glm(trout ~ burned_b_vs_unburned_u + wet_or_dry_in_2016_1, df_mod, family = binomial(link = "logit"))
summary(mod1)

plot(ggeffects::ggpredict(mod1, terms = ~wet_or_dry_in_2016_1+burned_b_vs_unburned_u ))


df_mod %>%
  group_by(burned_b_vs_unburned_u, wet_or_dry_in_2016_1) %>%
  summarize(count = n() )




















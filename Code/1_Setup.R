#--------------------------------
## Common libraries
#--------------------------------

library(tidyverse)



#----------------------------------
## Get data and clean
#----------------------------------

df <- read.csv("Data/Data_20240408.csv")


filter_cols <- read.csv("Data/Variables_of_interest.csv", header = F)

cols <- filter_cols$V1

df <- df[, names(df) %in% c(cols, "Burned..B..vs..unburned..U.", "Wet.or.dry.in.2016.1", "Q.estimate..m3.s.", "Total.inverts", "Thermal.index", "Average.chlorophyll.a.mg.m2." )] %>% 
  janitor::clean_names() %>%
  mutate(across(where(is.character), ~na_if(., "n/a")))
  
write.csv(df, "Data/cleaned_20240408.csv", row.names = F, quote = F)


#----------------------------------
## Some summary stats
#----------------------------------

df %>% 
  summarize(across(max_depth_m:total_trout, ~length(.x[!is.na(.x)]))) %>% 
  t() %>% as.data.frame() %>% rownames_to_column()





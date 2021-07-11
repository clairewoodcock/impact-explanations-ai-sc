
# Load custom data and functions ------------------------------------------
library(stringr)
r_src_dir <- "/src/r/"
source(str_c(getwd(), r_src_dir, "utils.R"))


# What proportion of people have had headaches? Answer, 96.54%
headache_df <- oii.freq(wholedata$Headache_Experience)

#People do seem to perceive GTA as more serious than Migraine 
# (answers were very serious, moderately serious and not very)


# Table 2 - Perceived disease seriousness ---------------------------------
percieved_seriousness <- wholedata %>% 
  group_by(Disease_Seriousness, Disease) %>% 
  count() %>% 
  rename(seriousness_n = n)
disease_sizes <- wholedata %>% 
  group_by(Disease) %>% 
  count() %>% 
  rename(disease_n = n)

percieved_seriousness <- left_join(
  percieved_seriousness, disease_sizes, by = "Disease")
percieved_seriousness <- percieved_seriousness %>% 
  mutate(perc_seriousness = 100* seriousness_n/disease_n)
percieved_seriousness <- percieved_seriousness %>% 
  select(-seriousness_n, - disease_n)
percieved_seriousness <- percieved_seriousness %>% 
  pivot_wider(names_from = Disease, values_from = perc_seriousness)
view(percieved_seriousness)

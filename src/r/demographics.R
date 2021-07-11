
# Load custom data and functions ------------------------------------------
library(stringr)
r_src_dir <- "/src/r/"
source(str_c(getwd(), r_src_dir, "utils.R"))


# Compute the age distribution --------------------------------------------

# Faceted by Explanation Type and Disease
ggplot(data=wholedata, aes(age)) + 
  geom_histogram(bins = 15, color = "black", fill = "gray") + 
  facet_grid(cols = vars(Treatment), rows = vars(Disease))

# Overall
ggplot(data=wholedata, aes(age)) + 
  geom_histogram(bins = 15, color = "black", fill = "gray")

# Demographic statistics
oii.summary(wholedata$age)
oii.freq(wholedata$`Employment Status`)


# Load custom functions and data ------------------------------------------
library(stringr)
r_src_dir <- "/src/r/"
source(str_c(getwd(), r_src_dir, "utils.R"))


# Specify data source -----------------------------------------------------
input_influence_likert <- getdataset(treatment = "Input Influence", questions_only = TRUE)
input_influence_withdata <- getdataset(treatment = "Input Influence", questions_only = FALSE)

# Initial exploration -----------------------------------------------------

#how many factors required? suggests 2, gives 2 eigen values above 1
parallel <- fa.parallel(input_influence_likert, fm = 'minres', fa = 'fa')

#2 factors causes q23 to double load
# Do oblimin on 3 factors with minres, BIC -150, cum variance 51.1%. 
# Q23, Q24 load separately.  Q23 also double loads on this. Decision to remove Q23.
factors <- obliminFA(input_influence_likert, factornumbers = 3, fm = 'minres')

# Table 4 - Factors with q23 removed ---------------------------------------
input_influence_likert_subset <- input_influence_likert %>% select(-Q23_L)

parallel <- fa.parallel(input_influence_likert_subset, fm = 'minres', fa = 'fa')
factors <- obliminFA(input_influence_likert_subset, factornumbers = 2, fm = 'minres')


# Create the factors as new variables -------------------------------------

input_influence_withdata$faith <- factors$scores[,1]
input_influence_withdata$comprehension <- factors$scores[,2]


# Check normality of created variables ------------------------------------

#faith is not normal (shapiro test is significant), plot shows it is negatively skewed, got a couple of strong outliers
shapiro.test(input_influence_withdata$faith)
ggqqplot(input_influence_withdata$faith)
hist(input_influence_withdata$faith, breaks = 200)

#comprehension is not normal (shapiro test is significant), plot shows it is negatively skewed, multimodel data
shapiro.test(input_influence_withdata$comprehension)
ggqqplot(input_influence_withdata$comprehension)
hist(input_influence_withdata$comprehension, breaks = 200)


#what are the levene's test results?

#p= 0.0021
leveneTest(input_influence_withdata$faith, input_influence_withdata$Disease)
#p=0.68
leveneTest(input_influence_withdata$comprehension, input_influence_withdata$Disease)


# Boxplots by disease type ------------------------------------------------

disease_fctr_levels = c("Migraine", "GTA")
input_influence_withdata$disease_fctr <- factor(input_influence_withdata$Disease, levels = disease_fctr_levels)

boxplot(formula = faith ~ disease_fctr, data = input_influence_withdata, xlab = "Disease", ylab = "faith")
boxplot(formula = comprehension ~ disease_fctr, data = input_influence_withdata, xlab = "Disease", ylab = "comprehension")

# Distribution plots by disease type --------------------------------------

# depth and comprehension are particularly interesting
ggplot(input_influence_withdata, aes(x=faith,fill = Disease)) + geom_density(alpha = 0.4)
ggplot(input_influence_withdata, aes(x=comprehension,fill = Disease)) + geom_density(alpha = 0.4)


# Comparison of means test under normality assumption ---------------------

# MANOVA, p=0.0014
outcomefactors <- cbind(input_influence_withdata$faith, input_influence_withdata$comprehension)
manova_model <- manova(outcomefactors ~ Disease, data = input_influence_withdata)
summary(manova_model, intercept = TRUE)

# Means and SDs
input_influence_withdata %>% 
  select(Disease, faith, comprehension) %>% 
  group_by(Disease) %>% 
  summarise(
    mean_c=mean(comprehension), sd_c=sd(comprehension),
    mean_f=mean(faith), sd_f=sd(faith))

# t-test (comprehension), p=0.2
comprehension_ttest <- t.test(
  input_influence_withdata$comprehension ~ input_influence_withdata$Disease)

# yuen's t-test for non-normal data (comprehension), p=0.155
yuen(comprehension ~ Disease, data = input_influence_withdata, tr = 0.2)

# t-test (faith), p=0.05
faith_ttest <- t.test(
  input_influence_withdata$faith ~ input_influence_withdata$Disease)

# yuen's t-test for non-normal data (faith), p=0.434
yuen(faith ~ Disease, data = input_influence_withdata, tr = 0.2)


# Effect sizes ------------------------------------------------------------
effect_size(
  input_influence_withdata, "comprehension", ttest_output = comprehension_ttest)
effect_size(
  input_influence_withdata, "faith", ttest_output = faith_ttest)

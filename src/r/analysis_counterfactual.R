
# Load custom functions and data ------------------------------------------
library(stringr)
r_src_dir <- "/src/r/"
source(str_c(getwd(), r_src_dir, "utils.R"))


# Specify data source -----------------------------------------------------
counterfactual_likert <- getdataset(treatment = "Counterfactual", questions_only = TRUE)
counterfactual_withdata <- getdataset(treatment = "Counterfactual", questions_only = FALSE)

# Initial exploration -----------------------------------------------------

#how many factors required? suggests 2, gives 2 eigen values above 1
parallel <- fa.parallel(counterfactual_likert, fm = 'minres', fa = 'fa')

# Table 4 - Factors -------------------------------------------------------

#do oblimin on 2 factors with minres, BIC -190, cum variance 47.2%, q23 looks a bit wonky
factors <- obliminFA(counterfactual_likert, factornumbers = 2, fm = 'minres')


# Create the factors as new variables -------------------------------------

counterfactual_withdata$faith <- factors$scores[,1]
counterfactual_withdata$comprehension <- factors$scores[,2]


# Check normality of created variables ------------------------------------

#faith is not normal (shapiro test is significant), plot shows it is negatively skewed with a couple of strong outliers
shapiro.test(counterfactual_withdata$faith)
ggqqplot(counterfactual_withdata$faith)
hist(counterfactual_withdata$faith, breaks = 100)

#comprehension is not normal (shapiro test is significant), plot shows it is negatively skewed, multimodel data
shapiro.test(counterfactual_withdata$comprehension)
ggqqplot(counterfactual_withdata$comprehension)
hist(counterfactual_withdata$comprehension, breaks = 100)

#what are the levene's test results?

#p= 0.13
leveneTest(counterfactual_withdata$faith, counterfactual_withdata$Disease)
#p=0.16
leveneTest(counterfactual_withdata$comprehension, counterfactual_withdata$Disease)

# Boxplots by disease type ------------------------------------------------

#box plot of the different treatments

disease_fctr_levels = c("Migraine", "GTA")
counterfactual_withdata$disease_fctr <- factor(counterfactual_withdata$Disease, levels = disease_fctr_levels)

boxplot(formula = faith ~ disease_fctr, data = counterfactual_withdata, xlab = "Disease", ylab = "faith")
boxplot(formula = comprehension ~ disease_fctr, data = counterfactual_withdata, xlab = "Disease", ylab = "comprehension")

# Distribution plots by disease type --------------------------------------
# depth and comprehension are particularly interesting
ggplot(counterfactual_withdata, aes(x=faith,fill = Disease)) + geom_density(alpha = 0.4)
ggplot(counterfactual_withdata, aes(x=comprehension,fill = Disease)) + geom_density(alpha = 0.4)

# Comparison of means test under normality assumption ---------------------

# MANOVA, p=0.053
outcomefactors <- cbind(counterfactual_withdata$faith, counterfactual_withdata$comprehension)
manova_model <- manova(outcomefactors ~ Disease, data = counterfactual_withdata)
summary(manova_model, intercept = TRUE)

# Means and SDs
counterfactual_withdata %>% 
  select(Disease, faith, comprehension) %>% 
  group_by(Disease) %>% 
  summarise(
    mean_c=mean(comprehension), sd_c=sd(comprehension),
    mean_f=mean(faith), sd_f=sd(faith))

# t-test (comprehension), p=0.03
comprehension_ttest <- t.test(
  counterfactual_withdata$comprehension ~ counterfactual_withdata$Disease)

# yuen's t-test for non-normal data (comprehension), p=0.0296
yuen(comprehension ~ Disease, data = counterfactual_withdata, tr = 0.2)

# t-test (faith), p=0.7
faith_ttest <- t.test(
  counterfactual_withdata$faith ~ counterfactual_withdata$Disease)


# Effect sizes ------------------------------------------------------------
effect_size(
  counterfactual_withdata, "comprehension", ttest_output = comprehension_ttest)
effect_size(
  counterfactual_withdata, "faith", ttest_output = faith_ttest)

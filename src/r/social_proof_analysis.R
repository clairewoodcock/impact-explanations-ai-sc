
# Load custom functions and data ------------------------------------------
library(stringr)
r_src_dir <- "/src/r/"
source(str_c(getwd(), r_src_dir, "utils.R"))


# Specify data source -----------------------------------------------------
social_proof_likert <- getdataset(treatment = "Social Proof", questions_only = TRUE)
social_proof_withdata <- getdataset(treatment = "Social Proof", questions_only = FALSE)

# Initial exploration -----------------------------------------------------

#how many factors required? suggests 3, gives 2 eigen values above 1
parallel <- fa.parallel(social_proof_likert, fm = 'minres', fa = 'fa')

# Table 4 - Factors -------------- ---------------------------------------

#do oblimin on 2 factors with minres, BIC -161, cum variance 57.7%. Q23 and Q24 load separately. 
#This makes sense as both Q23 and Q24 ask about satisfaction.
factors <- obliminFA(social_proof_likert, factornumbers = 3, fm = 'minres')


# Create the factors as new variables -------------------------------------

social_proof_withdata$faith <- factors$scores[,1]
social_proof_withdata$comprehension <- factors$scores[,2]
social_proof_withdata$depth <- factors$scores[,3]


# Check normality of created variables ------------------------------------

#faith is not normal (shapiro test is significant), plot shows it is negatively skewed with a couple of strong outliers
shapiro.test(social_proof_withdata$faith)
ggqqplot(social_proof_withdata$faith)
hist(social_proof_withdata$faith, breaks = 200)

#comprehension is not normal (shapiro test is significant), plot shows it is negatively skewed, multimodel data
shapiro.test(social_proof_withdata$comprehension)
ggqqplot(social_proof_withdata$comprehension)
hist(social_proof_withdata$comprehension, breaks = 200)

shapiro.test(social_proof_withdata$depth)
ggqqplot(social_proof_withdata$depth)
hist(social_proof_withdata$depth, breaks = 200)

#what are the levene's test results?

#p= 0.96
leveneTest(social_proof_withdata$faith, social_proof_withdata$Disease)
#p=0.45
leveneTest(social_proof_withdata$comprehension, social_proof_withdata$Disease)
#p=0.3
leveneTest(social_proof_withdata$depth, social_proof_withdata$Disease)

# Boxplots by disease type ------------------------------------------------
disease_fctr_levels = c("Migraine", "GTA")
social_proof_withdata$disease_fctr <- factor(social_proof_withdata$Disease, levels = disease_fctr_levels)

boxplot(formula = faith ~ disease_fctr, data = social_proof_withdata, xlab = "Disease", ylab = "faith")
boxplot(formula = comprehension ~ disease_fctr, data = social_proof_withdata, xlab = "Disease", ylab = "comprehension")
boxplot(formula = depth ~ disease_fctr, data = social_proof_withdata, xlab = "Disease", ylab = "depth")

# Distribution plots by disease type --------------------------------------
# depth and comprehension are particularly interesting
ggplot(social_proof_withdata, aes(x=faith,fill = Disease)) + geom_density(alpha = 0.4)
ggplot(social_proof_withdata, aes(x=comprehension,fill = Disease)) + geom_density(alpha = 0.4)
ggplot(social_proof_withdata, aes(x=depth,fill = Disease)) + geom_density(alpha = 0.4)


# Comparison of means test under normality assumption ---------------------

# MANOVA, p=0.049
outcomefactors <- cbind(social_proof_withdata$faith, social_proof_withdata$comprehension, social_proof_withdata$depth)
manova_model <- manova(outcomefactors ~ Disease, data = social_proof_withdata)
summary(manova_model, intercept = TRUE)

# Means and SDs
social_proof_withdata %>% 
  select(Disease, faith, comprehension, depth) %>% 
  group_by(Disease) %>% 
  summarise(
    mean_c=mean(comprehension), sd_c=sd(comprehension),
    mean_f=mean(faith), sd_f=sd(faith),
    mean_d=mean(depth), sd_d=sd(depth))

# t-test (comprehension), p=0.3
comprehension_ttest <- t.test(social_proof_withdata$comprehension ~ social_proof_withdata$Disease)

# yuen's t-test for non-normal data (comprehension), p=0.326
yuen(comprehension ~ Disease, data = social_proof_withdata, tr = 0.2)

# t-test (faith), p=0.1
faith_ttest <- t.test(social_proof_withdata$faith ~ social_proof_withdata$Disease)

# yuen's t-test for non-normal data (faith), p=0.0785
yuen(faith ~ Disease, data = social_proof_withdata, tr = 0.2)

# t-test (depth), p=0.4
depth_ttest <- t.test(social_proof_withdata$depth ~ social_proof_withdata$Disease)

# yuen's t-test for non-normal data (faith), p=0.515
yuen(depth ~ Disease, data = social_proof_withdata, tr = 0.2)


# Effect sizes ------------------------------------------------------------
effect_size(
  social_proof_withdata, "comprehension", ttest_output = comprehension_ttest)
effect_size(
  social_proof_withdata, "faith", ttest_output = faith_ttest)
effect_size(
  social_proof_withdata, "depth", ttest_output = depth_ttest)

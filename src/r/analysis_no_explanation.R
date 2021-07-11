
# Load custom functions and data ------------------------------------------
library(stringr)
r_src_dir <- "/src/r/"
source(str_c(getwd(), r_src_dir, "utils.R"))


# Specify data source -----------------------------------------------------
no_explanation_likert <- getdataset(treatment = "No Explanation", questions_only = TRUE)
no_explanation_withdata <- getdataset(treatment = "No Explanation", questions_only = FALSE)

#how many factors required? suggests 3, gives 2 eigen values above 1
parallel <- fa.parallel(no_explanation_likert, fm = 'minres', fa = 'fa')

#do oblimin on 2 factors with minres, BIC -161, cum variance 57.7%, questions 23 and 24 load seperately, these
#questions ask about satisfaction of the explanation type. kind of makes sense 
factors <- obliminFA(no_explanation_likert, factornumbers = 3, fm = 'minres')


# Create the factors as new variables -------------------------------------
no_explanation_withdata$faith <- factors$scores[,1]
no_explanation_withdata$comprehension <- factors$scores[,2]
no_explanation_withdata$depth <- factors$scores[,3]


# Check normality of created variables ------------------------------------
#faith is not normal (shapiro test is significant), plot shows it is negatively skewed with a couple of strong outliers
shapiro.test(no_explanation_withdata$faith)
ggqqplot(no_explanation_withdata$faith)
hist(no_explanation_withdata$faith, breaks = 200)

#comprehension is not normal (shapiro test is significant), plot shows it is negatively skewed, multimodel data
shapiro.test(no_explanation_withdata$comprehension)
ggqqplot(no_explanation_withdata$comprehension)
hist(no_explanation_withdata$comprehension, breaks = 200)

shapiro.test(no_explanation_withdata$depth)
ggqqplot(no_explanation_withdata$depth)
hist(no_explanation_withdata$depth, breaks = 200)

#what are the levene's test results?

#p=0.76
leveneTest(no_explanation_withdata$faith, no_explanation_withdata$Disease)
#p=0.038
leveneTest(no_explanation_withdata$comprehension, no_explanation_withdata$Disease)
#p=0.97
leveneTest(no_explanation_withdata$depth, no_explanation_withdata$Disease)


# Boxplots by disease type ------------------------------------------------
disease_fctr_levels = c("Migraine", "GTA")
no_explanation_withdata$disease_fctr <- factor(no_explanation_withdata$Disease, levels = disease_fctr_levels)

boxplot_faith <- boxplot(formula = faith ~ disease_fctr, data = no_explanation_withdata, xlab = "Disease", ylab = "faith")
boxplot_comprehension <- boxplot(formula = comprehension ~ disease_fctr, data = no_explanation_withdata, xlab = "Disease", ylab = "comprehension")
boxplot_depth <- boxplot(formula = depth ~ disease_fctr, data = no_explanation_withdata, xlab = "Disease", ylab = "depth")


# Distribution plots by disease type --------------------------------------
# depth and comprehension are particularly interesting
ggplot(no_explanation_withdata, aes(x=faith,fill = Disease)) + geom_density(alpha = 0.4)
ggplot(no_explanation_withdata, aes(x=comprehension,fill = Disease)) + geom_density(alpha = 0.4)
ggplot(no_explanation_withdata, aes(x=depth,fill = Disease)) + geom_density(alpha = 0.4)


# Comparison of means test under normality assumption ---------------------
# MANOVA, p=0.0062
outcomefactors <- cbind(no_explanation_withdata$faith, no_explanation_withdata$comprehension, no_explanation_withdata$depth)
manova_model <- manova(outcomefactors ~ Disease, data = no_explanation_withdata)
summary(manova_model, intercept = TRUE)

# Means and SDs
no_explanation_withdata %>% 
  select(Disease, faith, comprehension, depth) %>% 
  group_by(Disease) %>% 
  summarise(
    mean_c=mean(comprehension), sd_c=sd(comprehension),
    mean_f=mean(faith), sd_f=sd(faith),
    mean_d=mean(depth), sd_d=sd(depth))

# t-test (comprehension), p=0.002
ttest_comprehension <- t.test(no_explanation_withdata$comprehension ~ no_explanation_withdata$Disease)

# yuen's t-test for non-normal data (comprehension), p=0.00024
yuen(comprehension ~ Disease, data = no_explanation_withdata, tr = 0.2)

# t-test (faith), p=0.6
ttest_faith <- t.test(no_explanation_withdata$faith ~ no_explanation_withdata$Disease)

# yuen's t-test for non-normal data (faith), p=0.273
yuen(faith ~ Disease, data = no_explanation_withdata, tr = 0.2)

# t-test (depth), p=0.9
ttest_depth <- t.test(no_explanation_withdata$depth ~ no_explanation_withdata$Disease)

# yuen's t-test for non-normal data (depth), p=0.77
yuen(depth ~ Disease, data = no_explanation_withdata, tr = 0.2)


# Effect sizes ------------------------------------------------------------
effect_size(
  no_explanation_withdata, "comprehension", ttest_output = ttest_comprehension)
effect_size(
  no_explanation_withdata, "faith", ttest_output = ttest_faith)
effect_size(
  no_explanation_withdata, "depth", ttest_output = ttest_depth)

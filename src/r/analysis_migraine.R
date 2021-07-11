
# Load custom functions and data ------------------------------------------
library(stringr)
r_src_dir <- "/src/r/"
source(str_c(getwd(), r_src_dir, "utils.R"))

# Specify data source -----------------------------------------------------
migraine_likert <- getdataset(disease = "Migraine", questions_only = TRUE)
migraine_withdata <- getdataset(disease = "Migraine", questions_only = FALSE)

# Initial exploration -----------------------------------------------------

#how many factors required? suggests 3, gives 2 eigen values above 1
parallel <- fa.parallel(migraine_likert, fm = 'minres', fa = 'fa')

# Table 3 - Factors ------------------------------------------------------

#do oblimin on 3 factors with minres, BIC -178, cum variance 56%
factors <- obliminFA(migraine_likert, factornumbers = 3, fm = 'minres')

# Create the factors as new variables -------------------------------------

migraine_withdata$faith <- factors$scores[,1]
migraine_withdata$comprehension <- factors$scores[,2]
migraine_withdata$depth <- factors$scores[,3]

# Check normality of created variables ------------------------------------

#faith is not normal (shapiro test is significant), plot shows it is negatively skewed. Also has a few strong outliers
shapiro.test(migraine_withdata$faith)
ggqqplot(migraine_withdata$faith)
hist(migraine_withdata$faith, breaks = 200)

#comprehension is not normal (shapiro test is significant), plot shows it is negatively skewed, multimodel data
shapiro.test(migraine_withdata$comprehension)
ggqqplot(migraine_withdata$comprehension)
hist(migraine_withdata$comprehension, breaks = 200)

#depth is not normal (shapiro test is significant), plot shows it is negatively skewed, multimodel data
shapiro.test(migraine_withdata$depth)
ggqqplot(migraine_withdata$depth)
hist(migraine_withdata$depth, breaks = 20)

#what are the levene's test results?

#p=0.17
leveneTest(migraine_withdata$faith, migraine_withdata$Treatment)
#p=0.56
leveneTest(migraine_withdata$comprehension, migraine_withdata$Treatment)
#p=0.039
leveneTest(migraine_withdata$depth, migraine_withdata$Treatment)


# Boxplot by explanation type ---------------------------------------------

treatment_fctr_levels = c("No Explanation", "Counterfactual", "Model", "Social Proof")
migraine_withdata$treatment_fctr <- factor(migraine_withdata$Treatment, levels = treatment_fctr_levels)

boxplot(formula = faith ~ treatment_fctr, data = migraine_withdata, xlab = "Treatment", ylab = "faith")
boxplot(formula = comprehension ~ treatment_fctr, data = migraine_withdata, xlab = "Treatment", ylab = "comprehension")
boxplot(formula = depth ~ treatment_fctr, data = migraine_withdata, xlab = "Treatment", ylab = "depth")

# Distribution plots by explanation type ----------------------------------
# depth and comprehension are particularly interesting
ggplot(migraine_withdata, aes(x=faith,fill = Treatment)) + geom_density(alpha = 0.4)
ggplot(migraine_withdata, aes(x=depth,fill = Treatment)) + geom_density(alpha = 0.4)
ggplot(migraine_withdata, aes(x=comprehension,fill = Treatment)) + geom_density(alpha = 0.4)


# Comparison of means test under normality assumption ---------------------

# MANOVA, p=0.65
outcomefactors <- cbind(migraine_withdata$faith, migraine_withdata$comprehension, migraine_withdata$depth)
manova_model <- manova(outcomefactors ~ Treatment, data = migraine_withdata)
summary(manova_model, intercept = TRUE)


# Load custom functions and data ------------------------------------------
library(stringr)
r_src_dir <- "/src/r/"
source(str_c(getwd(), r_src_dir, "utils.R"))

# Specify data source -----------------------------------------------------
gta_likert <- getdataset(disease = "GTA", questions_only = TRUE)
gta_withdata <- getdataset(disease = "GTA", questions_only = FALSE)

# Initial exploration -----------------------------------------------------

#how many factors required? suggests 3, gives 2 eigen values above 1
parallel <- fa.parallel(gta_likert, fm = 'minres', fa = 'fa')


# Table 3 - Factors with Q24 removed --------------------------------------

gta_likert_subset <- select(gta_likert, -Q24_L)

#still suggests 2 factors
parallel <- fa.parallel(gta_likert_subset, fm = 'minres', fa = 'fa')

#removing q24 gives clearer loadings
factors <- obliminFA(gta_likert_subset, factornumbers = 2, fm = "minres")


# Create the factors as new variables -------------------------------------

gta_withdata$faith <- factors$scores[,1]
gta_withdata$comprehension <- factors$scores[,2]


# Check normality of created variables ------------------------------------

#faith is not normal (shapiro test is significant), plot shows it is negatively skewed and has a couple of strong outliers
shapiro.test(gta_withdata$faith)
ggqqplot(gta_withdata$faith)
hist(gta_withdata$faith, breaks = 200)

#comprehension is  normal (shapiro test is significant)
shapiro.test(gta_withdata$comprehension)
ggqqplot(gta_withdata$comprehension)
hist(gta_withdata$comprehension, breaks = 200)

#what are the levene's test results?

#p=0.053
leveneTest(gta_withdata$faith, gta_withdata$Treatment)
#p=0.2
leveneTest(gta_withdata$comprehension, gta_withdata$Treatment)


# Boxplot by explanation type ---------------------------------------------

treatment_fctr_levels = c("No Explanation", "Counterfactual", "Model", "Social Proof")
gta_withdata$treatment_fctr <- factor(gta_withdata$Treatment, levels = treatment_fctr_levels)

boxplot(formula = faith ~ treatment_fctr, data = gta_withdata, xlab = "Treatment", ylab = "faith")
boxplot(formula = comprehension ~ treatment_fctr, data = gta_withdata, xlab = "Treatment", ylab = "comprehension")


# Distribution plots by explanation type ----------------------------------

ggplot(gta_withdata, aes(x=faith,fill = Treatment)) + geom_density(alpha = 0.4)
ggplot(gta_withdata, aes(x=comprehension,fill = Treatment)) + geom_density(alpha = 0.4)


# Comparison of means test under normality assumption ---------------------

# MANOVA, p=0.086
outcomefactors <- cbind(gta_withdata$faith, gta_withdata$comprehension)
manova_model <- manova(outcomefactors ~ Treatment, data = gta_withdata)
summary(manova_model, intercept = TRUE)

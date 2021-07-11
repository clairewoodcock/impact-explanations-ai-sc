library(oii)
library(tidyverse)
library(psych)
library(reshape2)
library(stringr)
library(purrr)
library(car)
library(lubridate)
library(ggpubr)
library(pwr)
library(rrcov)
library(WRS2)
library(gmodels)

options(digits=3)


# Functions ---------------------------------------------------------------
# positive_shift <- function(x, epsilon = 0.0001) {
#   x - min(x) + epsilon
# }
# 
# apply_boxcox <- function(df){
#   # We need to make sure all values in the matrix are positive
#   df <- df %>% mutate_all(positive_shift)
#   # Compute the transformation for each column
#   power_transform <- df %>% powerTransform
#   # Get just the lambda of the transformation, but as a vector (because the bcPower function needs vectors)
#   lambda <- power_transform$lambda %>% as_vector
#   # The bcPower function also only transforms matrices, not dataframes, so to that conversion
#   df_matrix <- df %>% as.matrix
#   # Do the conversion
#   transformed_matrix <- bcPower(df_matrix, lambda = lambda)
#   # Turn it back into a tibble (data frame)
#   df <- transformed_matrix %>% as_tibble
# } 

# DataImport --------------------------------------------------------------
likertdata <- read_rds("data/publish/likertdata.rds")
wholedata <- read_rds("data/publish/wholedata.rds")

likertquestionstring <- c(
  'Q16_L', 'Q17_L', 'Q18_L', 'Q19_L', 'Q20_L', 'Q21_L', 'Q22_L', 'Q23_L', 
  'Q24_L', 'Q25_L', 'Q26_L', 'Q27_L', 'Q28_L')

# get data function ----------------------------------------------------------

december2019 <- dmy("1/12/2019")

getdataset <- function(
  disease = "Both", 
  questions_only = FALSE, 
  transformed_variables = FALSE, 
  treatment = "All"){
  #create a dataframe of likert data
  
  df <- likertdata
  
  #if the disease is migrain then update data frame to be filtered on Disease column
  if (disease == "Migraine") {
    df <- df %>% 
      filter(Disease == "Migraine")
  }
  
  #if the disease is GTA then update data frame to be filtered on Disease column
  if (disease == "GTA") {
    df <- df %>% 
      filter(Disease == "GTA")
  }
  
  #if explanation type is specified then filter
  if (treatment != "All") {
    df <- df %>% 
      filter(Treatment == treatment)
  }

  #if you want to just return the questions then filter out all these other columns
  
  if (questions_only == TRUE) {
   df <-  select(df, likertquestionstring) 
  }
  
  # if ((questions_only == FALSE) & (transformed_variables == TRUE)) {
  #   cat("youre trying to transform none questions")
  #   return (0)
  # }

  # 
  # if (transformed_variables == TRUE) {
  # df <- apply_boxcox(df)
  # }
  # 
  
  #once you've passed through all this, return the dataframe that has been built through this function
  return(df)
}

#rm(migraine_df, gta_df, likertdata)


# Varimax Analysis --------------------------------------------------------
# 
# varimaxPCA <- function(dataset, factornumbers, cutoff = 0.3) {
#   varimax_components <- principal(dataset, nfactors = factornumbers, rotate = "varimax", scores = TRUE)
#   varimax_components_table <- loadings(varimax_components)
#   print(varimax_components_table,cutoff = cutoff)
#   return(varimax_components)
# }


# Oblimin Analysis --------------------------------------------------------

obliminFA <- function(dataset, factornumbers, cutoff = 0.3, fm = "minres") {
  oblimin_components <- fa(
    dataset, nfactors = factornumbers, rotate = "oblimin", fm=fm)
  print(oblimin_components$loadings, cutoff = cutoff)
  fa.diagram(oblimin_components)
  return(oblimin_components)
}


# Function to calculate effect sizes --------------------------------------

effect_size <- function(df, dependentvariable, ttest_output){
  
  #get the means and standard deviations
  df_summary <- df %>% select(Disease, dependentvariable) %>% 
  group_by(Disease) %>% 
    summarise_all(list(~mean(.), ~sd(.), ~n()))
  print(df_summary)
  
  n1 <- df_summary$n[1]
  n2 <- df_summary$n[2]
  
  tvalue <- ttest_output$statistic[[1]]
  dfvalue <- ttest_output$parameter[[1]]
  
  #calculate cohen's d
  cohensdvalue <- tvalue * sqrt((n1+n2)/(n1*n2))
  cat("\n")
  cat("cohens d\n")
  cat(cohensdvalue)
  cat("\n")
  # 
  # #calculate r value
  # rvalue <- sqrt(tvalue^2/(tvalue^2+dfvalue))
  # cat("effect size r\n")
  # cat(rvalue)
  # cat("\n")
  # 
  pwr.t.test(n=n1, d = cohensdvalue, sig.level = ttest_output$p.value, 
             power = NULL, type = c("two.sample"), alternative = "two.sided")
             
}

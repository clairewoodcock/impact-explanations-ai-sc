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
library(digest)

options(digits=3)

# DataImport --------------------------------------------------------------

# Bring in qualtrics results
likertdata <- read_csv('data/actual/processed/studydata_with_likert.csv')
likertdata <- likertdata %>% mutate(EndDate = dmy_hm(likertdata$EndDate))

# Merge in prolific demographic info
wholedata <- rename(
  likertdata,
  Disease_Seriousness = "Q29", When_Use = "Q30", Headache_Experience = "Q31", 
  Used_SC_Recently = "Q32", Like_New_Tech = "Q33", Smartphone_Reliance = "Q34"
)

prolific_columns <- cols_only(
  participant_id = col_character(),
  age = col_guess(),
  `Country of Birth` = col_guess(), 
  `Employment Status` = col_guess(), 
  Sex = col_guess(), 
  `Student Status` = col_guess(),
  `First Language` = col_guess(), 
  `Current Country of Residence` = col_guess(), 
  Nationality = col_guess()
)

prolific7thoct <- read_csv('data/actual/prolific/7thoctober.csv', col_types = prolific_columns)
prolific19thoct <- read_csv('data/actual/prolific/19thoctober.csv', col_types = prolific_columns)
prolific2ndjan <- read_csv('data/actual/prolific/2ndJan.csv', col_types = prolific_columns)
prolific30thdec <- read_csv('data/actual/prolific/30thdecember.csv', col_types = prolific_columns)

prolific_whole <- bind_rows(prolific19thoct, prolific2ndjan, prolific30thdec, prolific7thoct)
prolific_whole <- prolific_whole %>% rename(PROLIFIC_PID = participant_id)

wholedata <- left_join(wholedata, prolific_whole, by = "PROLIFIC_PID")
wholedata <- select(
  wholedata, 
  -StartDate, -Status, -IPAddress, -Progress, -Finished, -RecordedDate, 
  -RecipientLastName, -RecipientFirstName, -RecipientEmail, -ExternalReference,
  -LocationLatitude, -LocationLongitude, -DistributionChannel,-UserLanguage)

wholedata$Disease_Seriousness[is.na(wholedata$Disease_Seriousness)] <- "Not very serious"
wholedata$Treatment[wholedata$Treatment == "Model"] <- "Input Influence"

likertdata <- select(
  likertdata, 
  Disease, Treatment, PROLIFIC_PID, EndDate, Duration = "Duration (in seconds)",
  Q16_L, Q17_L, Q18_L, Q19_L, Q20_L, Q21_L, Q22_L, Q23_L, Q24_L, Q25_L, Q26_L, 
  Q27_L, Q28_L)

likertdata$Treatment[likertdata$Treatment == "Model"] <- "Input Influence"

likertdata$uid <- sapply(likertdata$PROLIFIC_PID, digest, algo="md5")
likertdata <- likertdata %>% select(-PROLIFIC_PID)

likertdata %>% write_csv("data/publish/likertdata.csv")
likertdata %>% write_rds("data/publish/likertdata.rds")

wholedata$uid <- sapply(wholedata$PROLIFIC_PID, digest, algo="md5")
wholedata <- wholedata %>% select(-PROLIFIC_PID)

wholedata %>% write_csv("data/publish/wholedata.csv")
wholedata %>% write_rds("data/publish/wholedata.rds")

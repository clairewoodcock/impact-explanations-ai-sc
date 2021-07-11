
# Load custom data and functions ------------------------------------------
library(stringr)
r_src_dir <- "/src/r/"
source(str_c(getwd(), r_src_dir, "utils.R"))


# making a function which counts the response strings  -----------------------

#create a list of the responses people could make, as people could select multiple I need to comma seperate these strings

usage_response_options <- c("Any time I felt poorly", "If I felt moderately unwell",
                            "If I couldn't speak to a human clinician",
                            "In situations where I would currently Google my symptoms",
                            "I would never use this kind of symptom checker",
                            "Don't know")


get_counts_usage <- function(data_when_use) {
  #use the stringr package, call the split string function, define the strings as the when use column and split on ,
  when_use_split <- stringr::str_split(string = data_when_use, pattern = ",")
  
  #about to build a mega for loop, i'll need to write out the counts to a list...that's this
  counts_of_usage_responses <- list()
  
  #looping variable is called usage_response, it's temporary, but we're cycling through the list of responses I made L35
  #next i make a function which asks if that string is present in the vector of strings we're looking at
  for(usage_response in usage_response_options){
    is_response_present <- function(x){
      usage_response %in% x
    }
    
    #make a variable which uses the map function to loop through the strings of responses and applying the is response 
    #present function. map is kind of like a for loop. map is checking whether the usage_response i've pulled out
    #exists in each of the rows of data in my list, it gives a boolean. next line counts this
    responses_present <- when_use_split %>% purrr::map(is_response_present)
    responses_count <- responses_present %>% unlist() %>% sum()
    #need to populate our empty list. first part creates an entry in the list which is the name of the usage_response
    #then assign it the count that we calculated to get the final tally
    counts_of_usage_responses[[usage_response]]<-responses_count
  }
  
  # we can now create a tibble which shows us who said what
  usage_count_tibble <- tibble(response = names(counts_of_usage_responses), 
                               counts = counts_of_usage_responses %>% unlist())
}


all_treatments <- c("No Explanation", "Counterfactual", "Input Influence", "Social Proof")
all_diseases <- c("Migraine", "GTA")

#we're going to make a list of tibbles
usage_response_list <- list()
i = 1

#for each treatment in the list of treatments
for (treatment in all_treatments) {
  cat(treatment)
  cat("\n")
  
  #also nesting each disease in list of diseases
  for (disease in all_diseases) {
    cat(disease)
    
    #filter the dataset by the treatment and disease the loop is looking at right now
    when_use <- filter(wholedata, Disease == disease, Treatment == treatment)$When_Use
    
    #extract out the counts that you get for the response as a tibble
    usage_count <- get_counts_usage(data_when_use = when_use) %>% mutate(
      Disease = disease, Treatment = treatment)
   
    #and add the tibble to the list you created with a list position of i
    usage_response_list[[i]]<-usage_count
    
    #advance the loop
    i= i+1
    
    cat("\n")
  }
  
  cat("\n\n") 
}

#make a tibble of all of these outputs
usage_response_df <- bind_rows(usage_response_list)

# Table 6 - percentage of inclination to use ------------------------------

usage_response_overall <- usage_response_df %>% 
  group_by(response) %>% 
  summarise(count = sum(counts))

usage_response_overall$percentage <- 100 * usage_response_overall$count / 750

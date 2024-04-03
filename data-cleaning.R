###########################################
### DATA CLEANING: ST 704 FINAL PROJECT ###
### Author: Connor McNeill              ###
### Version: 03/21/2024 346p            ###
###########################################

# ---- Load in data & libraries ----
if (!require("sqldf")){
  install.packages("sqldf")
  library(sqldf)
}
if (!require("tidyverse")){
  install.packages("tidyverse")
  library(tidyverse)
}
if (!require("stringdist")){
  install.packages("stringdist")
  library(stringdist)
}

# Read in the datasets
diversity_school <- read_csv('data/diversity_school.csv')
historical_tuition <- read_csv('data/historical_tuition.csv')
tuition_cost <- read_csv('data/tuition_cost.csv')
tuition_income <- read_csv('data/tuition_income.csv')
salary_potential <- read_csv('data/salary_potential.csv')

# Merge datasets
tuitioncost_diversity <- dplyr::left_join(tuition_cost, diversity_school, 
                                      by = c("name", "state"))
salary_potential$state_name <- gsub("-", " ", salary_potential$state_name)

sptcdiv <- dplyr::inner_join(tuitioncost_diversity, salary_potential,
                             by=c("name" = "name", "state" = "state_name"))

length(unique(sptcdiv$name))
missing_names <- anti_join(salary_potential, sptcdiv,
          by=c("name" = "name", "state_name" = "state")) %>% 
  select(name, state_name)

salary_potential_2 = salary_potential
for (i in 1:dim(salary_potential)[1]) {
  name = salary_potential[i,2]
  if (name %in% missing_names$name) {
    name.fixed = tuition_cost$name[amatch(name, tuition_cost$name, maxDist=Inf)]
    print(paste0(name.fixed, " = ", name, "? (y/n)"))
    input = tolower(readline())
    if (input == 'y') {
      salary_potential_2[i,2] = name.fixed
    } else {
      print("Enter name: ")
      salary_potential_2[i,2] = readline()
    }
    which(salary_potential_2$name == "Linfield College")
  }salary_potential_2$name[681] = "Linfield College - Adult"
}

salary_potential_2$name[681] = "Linfield College - Adult"
view(cbind(salary_potential$name, salary_potential_2$name))

dataset_1 <- dplyr::inner_join(tuitioncost_diversity, salary_potential_2,
                              by=c("name" = "name", "state" = "state_name"))
save(dataset_1, file="data/dataset_1.RData")
save(salary_potential_2, file="data/salary_potential_fixed.RData")

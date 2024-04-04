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
  }
}

salary_potential_2$name[681] = "Linfield College - Adult"
view(cbind(salary_potential$name, salary_potential_2$name))

dataset_1 <- dplyr::inner_join(tuitioncost_diversity, salary_potential_2,
                              by=c("name" = "name", "state" = "state_name"))
save(dataset_1, file="data/dataset_1.RData")
save(salary_potential_2, file="data/salary_potential_fixed.RData")
load("data/salary_potential_fixed.RData")

scorecard_data <- read_csv("data/Most-Recent-Cohorts-Institution.csv")

final_data <- inner_join(dataset_1, scorecard_data,
                         by = c("name" = "name", "state_code" = "state_abbr"))
missing_names2 <- setdiff(unique(dataset_1$name), unique(final_data$name))

states.abbr <- tuition_cost %>% select(state, state_code) %>% unique()

updated.names <- data.frame(right = missing_names2,
                            old = character(length(missing_names2)))
# Take the name that is missing
for (i in 1:length(updated.names$right)) {
  right.name = updated.names$right[i]
  potential.match = scorecard_data$name[amatch(right.name, scorecard_data$name,
                                               maxDist = Inf)] 
  print(paste0(potential.match, " = ", right.name, "? (y/n)"))
  input = tolower(readline())
  if (input == 'y') {
    updated.names$old[i] = potential.match
  } else {
    print("Enter name: ")
    updated.names$old[i] = readline()
  }
}


names.match = data.frame(original = salary_potential_2$name,
                         matched = character(length(salary_potential_2$name)))

for (i in 1:length(names.match$original)) {
  name = names.match$original[i]
  if (name %in% updated.names$right) {
    name = updated.names$old[which(name == updated.names$right)]
  }
  names.match$matched[i] = name
}

salary_potential_merged %>% 

salary_potential_merged = inner_join(salary_potential_2, names.match,
                                     by=c("name" = "original"), relationship = "many-to-many")
salary_potential_mergeda = inner_join(salary_potential_merged, states.abbr,
                                      by = c("state_name" = "state"))
updated_salary_potential = salary_potential_mergeda %>% distinct()
salarypotscore = inner_join(updated_salary_potential, scorecard_data,
                              by=c("matched" = "name", "state_code" = "state_abbr"))
final.data = inner_join(tuitioncost_diversity, salarypotscore,
                        by=c("name" = "name", "state" = "state_name"))
final.date.wide = final.data %>% pivot_wider(names_from = 'category',
                                  values_from = 'enrollment')
final.dataset = final.date.wide
save.image("work_apr4.RData")
save(final.dataset, file="data/college_data.RData")
write_csv(final.dataset, file="data/college_data_cleaned.csv")                                  

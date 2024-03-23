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
if (!require("readxl")){
  install.packages("readxl")
  library(readxl)
}


# install.packages("devtools")
# library("devtools")
# library("linelist")
# devtools::install_github("reconhub/linelist")
# devtools::install_github("reconhub/reportfactory")
# devtools::install_github("reconhub/epicontacts")

setwd("C:/Users/ELISHAARE/Desktop/Challenge_assignment/Elisha_Jeremy_Challenge_Assignment")
ebola_linelist_data <- read.csv(file = "ebola_2.csv") #importing raw data
library(devtools)
library(linelist)
library(tidyverse)
library(lubridate)
ebola_linelist_data_clean <- clean_data(ebola_linelist_data) 
ebola_linelist_data_cleann <-  clean_dates(ebola_linelist_data) 
ebola_linelist_data_cleann %>% 
  arrange(onsetDate) %>% 
  mutate (delay_onset_report = reportDate-onsetDate) 


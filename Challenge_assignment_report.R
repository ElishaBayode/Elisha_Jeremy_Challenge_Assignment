# install.packages("devtools")
# library("devtools")
# # library("linelist")
#  devtools::install_github("reconhub/linelist")
#  devtools::install_github("reconhub/reportfactory")
#  devtools::install_github("reconhub/epicontacts")

rm(list = ls())

# *set required packages to automatically install if necessary
library(devtools)
library(linelist)
library(tidyverse)
library(lubridate)
library(incidence)
library(envDocument)

# FILE <- file.choose()
# DIR  <- dirname(FILE)
# *set automatic prompt
setwd("C:/Users/ELISHAARE/Desktop/Challenge_assignment/Elisha_Jeremy_Challenge_Assignment")
setwd("C:/Users/jeremyb/Desktop/Smarties/Elisha_Jeremy_Challenge_Assignment")

##Define functions

calculateCarefulCaseFatality <- function(dataset, thresholdTimeToDeath, rounded=TRUE, asPercentage= TRUE){
  lastReportDate = last(dataset$reportDate)
  casesConsidered <- dataset %>%
    subset(as.numeric(difftime(lastReportDate,onsetDate)) >= thresholdTimeToDeath)
  nCases = nrow(casesConsidered)
  nDeaths = nCases - sum(is.na(casesConsidered$deathDate))
  caseFatalityRate = nDeaths/nCases
 
  #* if (nrow(casesConsidered==0)){
  #   print("There are no cases in this dataset which are eligible.")
  #   return(NA)
  # }
 
  if(rounded){
    if(asPercentage){  
      return(round(100*caseFatalityRate))}
    else{
      return(round(100*caseFatalityRate)/100)
    }
  }
  if(asPercentage){
    return(100*caseFatalityRate)}
  return(caseFatalityRate)
}


calculateCrudeCaseFatality <- function(dataset, rounded=TRUE, asPercentage=TRUE){
  nCases = nrow(dataset)
  nDeaths = nCases - sum(is.na(dataset$deathDate))
  caseFatalityRate = nDeaths/nCases
  
  if(rounded){
    if(asPercentage){
      return(round(100*caseFatalityRate))}
    else{
      return(round(100*caseFatalityRate)/100)
    }
  }
  if(asPercentage){
    return(100*caseFatalityRate)}
  return(caseFatalityRate)
}


##

ebolaData <- read.csv(file = "ebola_2.csv") #importing raw data
ebolaDataDateFormatted <- clean_dates(ebolaData)
# ebola_linelist_data_cleann <-  clean_dates(ebola_linelist_data)


transformedData <- ebolaDataDateFormatted %>%
                                    arrange(onsetDate) %>%
                                    mutate (reportingDelay = difftime(reportDate, onsetDate))

transformedData <- transformedData %>%
                                    mutate(timeBeforeDeath = difftime(deathDate,onsetDate))


# hist(as.numeric(transformedData$timeBeforeDeath),breaks = 10)
hist(as.numeric(difftime(transformedData$reportDate,transformedData$onsetDate)))
# pick a threshold timeBeforeDeath
  conservativeThreshold <- max(as.numeric(transformedData$timeBeforeDeath),na.rm = TRUE)
  liberalThreshold <- mean(as.numeric(transformedData$timeBeforeDeath),na.rm = TRUE)+2*sd(as.numeric(transformedData$timeBeforeDeath),na.rm = TRUE)
# subset all cases for which as.numeric(difftime(lastReportingDate,onsetDate)) >= threshold
# for these cases calculate death rate
thresholdTimeSinceOnset = liberalThreshold
lastReportDate = last(transformedData$reportDate)

casesWhichWouldHaveDiedIfTheyWereGoingToDie <- transformedData %>%
                                              subset(as.numeric(difftime(lastReportDate,onsetDate)) >= thresholdTimeSinceOnset)


nCasesWouldHaveDied = nrow(casesWhichWouldHaveDiedIfTheyWereGoingToDie)
nDeathsWouldHaveDied = nCasesWouldHaveDied - sum(is.na(casesWhichWouldHaveDiedIfTheyWereGoingToDie$deathDate))
caseFatalityRate = nDeathsWouldHaveDied/nCasesWouldHaveDied
roundedCaseFatalityRate = round(100*caseFatalityRate)

calculateCarefulCaseFatality(transformedData,liberalThreshold)


# nCases = nrow(transformedData)
# nDeaths = nCases - sum(is.na(transformedData$deathDate))
# crudeCaseFatalityRate = nDeaths/nCases #* mention this as to why we've calculated it this way

# nrow(casesWhichWouldHaveDiedIfTheyWereGoingToDie)/nrow(transformedData)




# typeof(ebolaDataDateFormatted$onsetDate)
# difftime(transformedData$deathDate,transformedData$onsetDate)

lengthOfOutbreak = abs(difftime(first(transformedData$onsetDate),last(transformedData$onsetDate)))

ebolaCasesConfirmed <- transformedData %>%
  filter(status=='confirmed')

ebolaCasesSuspected <- transformedData %>%
  filter(status=='suspected')

ebolaCasesProbable <- transformedData %>%
  filter(status=='probable')

# should we count dates on x axis from outbreak onset or in calendar date
# ** need titles, x label
# * make incidence plots pretty
plot(incidence(transformedData$onsetDate,interval = 1),title("Incidence"))

plot(incidence(transformedData$onsetDate,interval = 7))

plot(incidence(ebolaCasesConfirmed$onsetDate,interval=1))
plot(incidence(ebolaCasesSuspected$onsetDate,interval=1))
plot(incidence(ebolaCasesProbable$onsetDate,interval=1))

##cumulative incidence plot
plot(cumulate(incidence(transformedData$onsetDate,interval = 1)))
weeklyIncidence = incidence(transformedData$onsetDate,interval=7)
weeklyIncidence$dates
model2 = fit(incidence(transformedData$onsetDate,interval=7),split = weeklyIncidence$dates[which.max(weeklyIncidence$counts)])
plot(model2)
model2

model = fit(incidence(transformedData$onsetDate,interval=7))
plot(model) + geom_point(aes(x=incidence(transformedData$onsetDate,interval=7)$dates,y=incidence(transformedData$onsetDate,interval=7)$counts,col='red')) + theme(legend.position = "none")

plot(model2) + geom_point(aes(x=incidence(transformedData$onsetDate,interval=7)$dates,y=incidence(transformedData$onsetDate,interval=7)$counts,col='red')) + theme(legend.position = "none")

plot(incidence(transformedData$onsetDate))

model

model2$before$info$r
model2$before$info$r.conf[1] #2.5%
model2$before$info$r.conf[2] #95%

model2$after$info$r
model2$after$info$r.conf[1] #2.5%
model2$after$info$r.conf[2] #95%

model$info$r
model$info$r.conf[1] #2.5%
model$info$r.conf[2] #95%


incidence(transformedData$onsetDate)$dates
last(transformedData$onsetDate)


# plot((seq(1,length(weeklyIncidence$dates[1:6]))), weeklyIncidence$counts[1:6])

# fit = lm((weeklyIncidence$counts[1:6]) ~ log(seq(1,length(weeklyIncidence$dates[1:6])),base=2))

# fit$coefficients

# Plot the fitted line
# lines(seq(1,length(weeklyIncidence$dates[1:6])), seq(1,length(weeklyIncidence$dates[1:6])) ^ fit$coefficients[2], col = "red")

# plot(model)

## case fatalities by category

## The case fatality rate is calculated as follows: # We assume that cases without arecorded death date (death date = NA) for more than $thresholdTimeSinceOnset$ after date of onset have recovered.
# This leaves 60\% of the cases to date which occured early enough (date of onset at least $thresholdTimeSinceOnset$ before the last date of reporting) to be included in the case fatality rate.
# The crude case fatality for cases and deaths reported to date is calculateCrudeCaseFatality(transformedData)
calculateCarefulCaseFatality(transformedData, thresholdTimeToDeath = liberalThreshold)
# The case fatality by `status` is calculated:

calculateCarefulCaseFatality(ebolaCasesConfirmed,thresholdTimeToDeath = liberalThreshold)
calculateCarefulCaseFatality(ebolaCasesProbable,thresholdTimeToDeath = liberalThreshold)
calculateCarefulCaseFatality(ebolaCasesSuspected,thresholdTimeToDeath = liberalThreshold)

# ** do we want calculate confidence intervals on case fatalities

# README stuff
# required packages
# Most recent R, package, and Rstudio versions confirmed working


incidenceToDate = incidence(transformedData$onsetDate)
max(incidenceToDate$counts)
incidenceToDate$dates[which.max(incidenceToDate$counts)]

print(incidenceToDate$counts)
print(incidenceToDate[1,1])

weeklyIncidenceToDate = incidence(transformedData$onsetDate,interval = 7)
max(weeklyIncidenceToDate$counts)
which.max(weeklyIncidenceToDate$counts)

ensure_library = function (lib.name){
  x = require(lib.name, quietly = TRUE, character.only = TRUE)
  if (!x) {
    install.packages(lib.name, dependencies = TRUE, quiet = TRUE)
    x = require(lib.name, quietly = TRUE, character.only = TRUE)
  }
  x
}
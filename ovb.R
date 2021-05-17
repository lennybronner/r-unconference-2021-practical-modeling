library(readr)
library(stringr)
library(dplyr)
library(cdlTools)
library(tidyr)

setwd("~/Documents/r-unconference-2021")
source('base_data.R')

head(cces)

##############
## Show OVB ##
##############

model = lm(voted_trump ~ factor(race),
           data=cces)

summary(model)

model = lm(voted_trump ~ factor(race) + factor(educ) + factor(gender) + factor(age_bucket) + factor(state_abb), 
           data=cces)

summary(model)

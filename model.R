library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(cdlTools)
library(ROCR)

setwd("~/Documents/r-unconference-2021")
source('base_data.R')

head(cces)

CUTOFF = 0.471

######################
## Evaluating model ##
######################

cces_shuffled = sample(cces) 
last_train_index = floor(0.8 * nrow(cces_shuffled))
cces_train = cces[1:last_train_index,]
cces_test = cces[last_train_index:nrow(cces_shuffled),]

model = glm(voted_trump ~ factor(state_abb) + factor(age_bucket) + factor(race) + factor(gender),
            data=cces_train,
            family='binomial')

y_hat_test = predict(model, newdata=cces_test, type='response')
pred = prediction(y_hat_test, cces_test$voted_trump)
acc = performance(pred, "acc")
print(max(acc@y.values[[1]]))

position_cutoff = which(acc@y.values[[1]] == max(acc@y.values[[1]]))
cutoff = acc@x.values[[1]][position_cutoff]
print(cutoff)

####################
## Applying Model ##
####################

model = glm(voted_trump ~ factor(state_abb) + factor(age_bucket) + factor(race) + factor(gender),
           data=cces,
           family='binomial')

summary(model)

head(nc_voterfile)

#nc_voterfile = read.csv('data/nc_voterfile_processed.csv') %>%
#  drop_na(race, gender, age_bucket)

preds_voted = predict(model, nc_voterfile %>% filter(voted_2020 == 1), type='response')

sum(preds_voted > CUTOFF) / length(preds_voted) # actual percentage was 49.93%

preds = predict(model, nc_voterfile , type='response')  

sum(preds > CUTOFF) / length(preds)

################
## Robustness ##
################

sum(preds > 0.5) / length(preds)

sum(preds > 0.4) / length(preds)

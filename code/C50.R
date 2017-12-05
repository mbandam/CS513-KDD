# Brendan Reis

install.packages("C50")

rm(list=ls())

set.seed(666)

source("./DataCleaning.R")
LifeInsurance_training_data$Response <- as.factor(LifeInsurance_training_data$Response)

library(C50)

#LifeInsurance_training_data = subset(LifeInsurance_training_data, select=c(2,4,5,9,10,11,12,13,14,16,18,19,21,34,39,40,80, 128))

accuracy_sum <- 0
rounds <- 3

modelTime <- 0

for(i in 1:rounds){
  print(i)
  indices <- sample(c(TRUE, FALSE), nrow(LifeInsurance_training_data), replace=TRUE, prob=c(0.5, 0.5))
  train <- LifeInsurance_training_data[indices, ]
  test <- LifeInsurance_training_data[!indices, ]
  
  modelStart <-Sys.time()
  # fit model
  model <- C5.0(formula=Response~., data=train, trials=2)
  modelTime = modelTime + Sys.time() - modelStart
  
  #print(summary(model))
  
  test_data <- subset(test, select = -Response)
  test_responses <- test$Response
  
  predictions <- predict(model, test_data)
  
  accuracy = sum(predictions == test_responses) / length(predictions)
  
  print(accuracy)
  accuracy_sum <- accuracy_sum + accuracy
}

print(accuracy_sum / rounds)

print(modelTime / rounds)
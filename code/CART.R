# Brendan Reis

install.packages("RColorBrewer")
install.packages("rattle")
install.packages("rpart")

rm(list=ls())

set.seed(666)

source("./DataCleaning.R")

library(RColorBrewer)
library(rattle)
library(rpart)

accuracy_sum <- 0

rounds <- 3

modelTime <- 0

for(i in 1:rounds){
  print(i)
  indices <- sample(c(TRUE, FALSE), 19765, replace=TRUE, prob=c(0.5, 0.5))
  train <- LifeInsurance_training_data[indices, ]
  test <- LifeInsurance_training_data[!indices, ]
  
  modelStart <-Sys.time()
  # fit model
  model <- rpart(Response~., data=train)
  modelTime = modelTime + Sys.time() - modelStart
  
  #summary(model)
  
  #plot = fancyRpartPlot(model)
  
  test_data <- subset(test, select = -Response)
  test_responses <- test$Response
  
  predictStart <- Sys.time()
  predictions <- predict(model, test_data)
  confusion_matrix <- table(test_responses, predictions)
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print(accuracy)
  accuracy_sum = accuracy_sum + accuracy
  predictTime <- Sys.time() - predictStart
  
  print(modelTime)
  print(predictTime)
}

print(accuracy_sum / rounds)
print(modelTime / rounds)

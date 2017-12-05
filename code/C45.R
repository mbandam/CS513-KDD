# Brendan Reis

install.packages("rJava")
install.packages("RWeka")

rm(list=ls())

set.seed(666)

source("./DataCleaning.R")
LifeInsurance_training_data$Response <- as.factor(LifeInsurance_training_data$Response)

library(rJava)
library(RWeka)

accuracy_sum = 0

rounds <- 3

modelTime <- 0

for(i in 1:rounds){
  print(i)
  indices <- sample(c(TRUE, FALSE), 19765, replace=TRUE, prob=c(0.5, 0.5))
  train <- LifeInsurance_training_data[indices, ]
  test <- LifeInsurance_training_data[!indices, ]
  
  modelStart <-Sys.time()
  # fit model
  model <- J48(Response~., data=train)
  
  modelTime = modelTime + Sys.time() - modelStart
  
  #summary(model)
  
  test_data <- subset(test, select = -Response)
  test_responses <- test$Response
  
  predictions <- predict(model, test_data)
  confusion_matrix <- table(test_responses, predictions)
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print(accuracy)
  accuracy_sum <- accuracy_sum + accuracy
}

print(accuracy_sum / rounds)
print(modelTime / rounds)


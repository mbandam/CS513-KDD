# Brendan Reis

install.packages("RColorBrewer")
install.packages("rattle")
install.packages("rpart")

rm(list=ls())
source("./DataCleaning.R")

library(RColorBrewer)
library(rattle)
library(rpart)

accuracy_sum <- 0

for(i in 1:10){
  print(i)
  indices <- sample(c(TRUE, FALSE), 19765, replace=TRUE, prob=c(0.8, 0.2))
  train <- LifeInsurance_training_data[indices, ]
  test <- LifeInsurance_training_data[!indices, ]
  
  # fit model
  model <- rpart(Response~., data=train)
  
  #summary(model)
  
  #plot = fancyRpartPlot(model)
  
  test_data <- subset(test, select = -Response)
  test_responses <- test$Response
  
  predictions <- predict(model, test_data)
  confusion_matrix <- table(test_responses, predictions)
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print(accuracy)
  accuracy_sum = accuracy_sum + accuracy
}

print(accuracy_sum / 10)


# removing all the objects
rm(list=ls())

set.seed(666)

library(class)
library(kknn)

path <- "/Users/KLahmy/Documents/stevens/CS-513/Final Project/CS513-KDD-master/Data/"

source("./DataCleaning.R")

# loading the training data
#LifeInsurance_training_data <- read.csv("../Data/train.csv",na.strings=c("",NA))
#col 13
#LifeInsurance_training_data$Employment_Info_1[is.na(LifeInsurance_training_data$Employment_Info_1)]<-mean(LifeInsurance_training_data$Employment_Info_1, na.rm = TRUE)
#Col 16
#LifeInsurance_training_data$Employment_Info_4[is.na(LifeInsurance_training_data$Employment_Info_4)]<-mean(LifeInsurance_training_data$Employment_Info_4, na.rm = TRUE)
#col 18
#LifeInsurance_training_data$Employment_Info_6[is.na(LifeInsurance_training_data$Employment_Info_6)]<-mean(LifeInsurance_training_data$Employment_Info_6, na.rm = TRUE)
#col 39
#LifeInsurance_training_data$Medical_History_1[is.na(LifeInsurance_training_data$Medical_History_1)]<-median(LifeInsurance_training_data$Medical_History_1, na.rm = TRUE)


idx=seq(from=1,to=nrow(LifeInsurance_training_data),by=5)


test<-LifeInsurance_training_data[idx,]
training<-LifeInsurance_training_data[-idx,]

training <- training[,c(-1,-3,-15,-30,-35,-36,-37,-38,-48,-53,-62,-70)]
test <- test[,c(-1,-3,-15,-30,-35,-36,-37,-38,-48,-53,-62,-70)]


#training <- training[,c(4,5,9,10,11,12,13,14,16,18,19,21,26,28,29,31,34,39,40,67,80)]

#test <- test[,c(4,5,9,10,11,12,13,14,16,18,19,21,26,28,29,31,34,39,40,67,80)]


test$Response <- as.factor(test$Response)
training$Response <- as.factor(training$Response)

colnames(test)
train_data <- subset(training, select = -Response)
train_responses <- training$Response
test_data <- subset(test, select = -Response)
test_responses <- test$Response

predict<-knn(train_data, test_data, train_responses, k=5)

print(predict)

#table(Actual=test[test_responses],Prediction=predict)

#wrong<-results[,totalCols]!=results[,totalCols + 1]

#rate<-sum(wrong)/length(wrong)

print(sum(test_responses == predict)/length(test_responses))


###################################
#RESULTS:
###(-1,-3,-30,-35,-36,-37,-38,-39,-48,-53,-62,-70) - 0.6632988
###(-1,-3,-30,-35,-36,-37,-38,-48,-53,-62,-70) 0.6792961

## after removing all Medical keyword columns - 0.6860318


## 2,4,5,9,10,11,12,13,14,16,18,19,21,34,39,40,80 - 0.7133956

##Adding all product info
## 2,4,5,6,7,8,9,10,11,12,13,14,16,18,19,21,34,39,40,80 - 0.7177738


#Steps for the data analysis:
# 1- choose all
# 2 - start removing columns:
#   2A - Remove all columns that have a lot of missing values
#   2B - Remove all keyword columns
# 3 - go the other way around - start from some columns and add them
# 4 - check columns with 2 options and see how they divide, if the division is 80/20 or less, ignore, otherwise add and see what's the results
#Columns 20,22,23,25,60,71,76, can be ignored, only two options and most people chose one option
# 5 - check columns with 3 options and see how they divide, if the division is 80/20 or less for one/two options, ignore, otherwise add and see what's the results
#Columns   ,8,19?,27,41,43,44,45,46,49,50,51,52,54?,55,56,57,58,59,63?,64?,65,66,68,69,72,73,74?,75,77,78 can be ignored, only two options and most people chose one option



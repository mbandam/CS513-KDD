# removing all the objects
rm(list=ls())

# Loading the prudential dataset
LifeInsurance_data <- read.csv("/Users/mounikabandam/Documents/Stevens/CS 513 Knowledge Dis and Data mining/Project/Data/train.csv",na.strings=c("",NA))

# Removing the dummy variables as facing problem with the model running
LifeInsurance_data <- LifeInsurance_data[,-c(80:127)]

# Removng columns which are not required for the model(reason mentioned in DataCleaning file)
LifeInsurance_data <- LifeInsurance_data[,c(-1,-3,-30,-35,-36,-37,-38,-48,-53,-62,-70)]

# code for handling the missing values in traing data
LifeInsurance_data$Employment_Info_1[is.na(LifeInsurance_data$Employment_Info_1)]<-mean(LifeInsurance_data$Employment_Info_1, na.rm = TRUE)

LifeInsurance_data$Employment_Info_4[is.na(LifeInsurance_data$Employment_Info_4)]<-mean(LifeInsurance_data$Employment_Info_4, na.rm = TRUE)

LifeInsurance_data$Medical_History_1[is.na(LifeInsurance_data$Medical_History_1)]<-median(LifeInsurance_data$Medical_History_1, na.rm = TRUE)

LifeInsurance_data$Employment_Info_6[is.na(LifeInsurance_data$Employment_Info_6)]<-mean(LifeInsurance_data$Employment_Info_6, na.rm = TRUE)

# Getting the index values starting form 1 and then every fifth record(1,6,11,16,...)
index<-seq(from=1,to=nrow(LifeInsurance_data),by=5)

# Splitting the data into traing and test
# Store every fifth record in a "test" dataset starting with the first record
test<-LifeInsurance_data[index,]
# Store the rest in the "training" dataset
training<-LifeInsurance_data[-index,]

# installing and loading the package nnet
install.packages('nnet')
library(nnet)

# logistic regression model
model <- multinom (Response~., data = training)
# summary(model)

predicted_scores <- predict(model, test, type = 'probs')
predicted_class <- predict (model, test)

#confusion matrix
table(predicted_class, test$Response)

# Error rate
mean(as.character(predicted_class) != as.character(test$Response))

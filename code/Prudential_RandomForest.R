# removing all the objects
rm(list=ls())

# Loading the prudential dataset
LifeInsurance_data <- read.csv("/Users/mounikabandam/Documents/Stevens/CS 513 Knowledge Dis and Data mining/Project/Data/train.csv",na.strings=c("",NA))

# converting the Class column to factor Column
class(LifeInsurance_data$Response) #integer
LifeInsurance_data$Response<-as.factor(LifeInsurance_data$Response)

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

# installing and loading the package randomForest
install.packages("randomForest")
library('randomForest')

# applying the random forest
fit <- randomForest(Response~.,data = training, importance = TRUE, ntree= 300) # no of trees checked from 0 to 10000 and got the good result at 300, so i am using the 300.
importance(fit)

# tells about which variables are important
varImpPlot(fit)

# Predicting the test data
Prediction <- predict(fit,test)

#confusion Matrix
table(actual=test[,10], Prediction)

# Error rate
wrong<- (test[,10]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate

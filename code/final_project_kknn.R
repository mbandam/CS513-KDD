# removing all the objects
rm(list=ls())

set.seed(666)

library(class)
library(kknn)

path <- "/Users/KLahmy/Documents/stevens/CS-513/Final Project/CS513-KDD-master/Data/"

# loading the training data
LifeInsurance_training_data <- read.csv(paste(path,"train.csv",sep=""),na.strings=c("",NA))

LifeInsurance_training_data$Employment_Info_1[is.na(LifeInsurance_training_data$Employment_Info_1)]<-mean(LifeInsurance_training_data$Employment_Info_1, na.rm = TRUE)
LifeInsurance_training_data$Employment_Info_4[is.na(LifeInsurance_training_data$Employment_Info_4)]<-mean(LifeInsurance_training_data$Employment_Info_4, na.rm = TRUE)
LifeInsurance_training_data$Employment_Info_6[is.na(LifeInsurance_training_data$Employment_Info_6)]<-mean(LifeInsurance_training_data$Employment_Info_6, na.rm = TRUE)
LifeInsurance_training_data$Medical_History_1[is.na(LifeInsurance_training_data$Medical_History_1)]<-median(LifeInsurance_training_data$Medical_History_1, na.rm = TRUE)


idx=seq(from=1,to=nrow(LifeInsurance_training_data),by=5)


test<-LifeInsurance_training_data[idx,]
training<-LifeInsurance_training_data[-idx,]


#training <- training[,c(-1,-3,-30,-35,-36,-37,-38,-48,-53,-62,-70)]
#test <- test[,c(-1,-3,-30,-35,-36,-37,-38,-48,-53,-62,-70)]


training <- training[,c(4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,21,24,26,28,29,31,32,33,34,40,42,47,54,61,63,64,67,74,79,80)]

test <- test[,c(4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,21,24,26,28,29,31,32,33,34,40,42,47,54,61,63,64,67,74,79,80)]

test$Response <- as.factor(test$Response)
training$Response <- as.factor(training$Response)

predict_k5 <- kknn(formula=Response~., training, test, k=25)

fit <- fitted(predict_k5)

print(sum(test$Response == fit)/length(test$Response))


#Almost all K= 5 0.3473099, K=25 0.424097
#SOME K = 25 0.4208975






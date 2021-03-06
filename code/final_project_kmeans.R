# removing all the objects
rm(list=ls())

set.seed(666)

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

km <- kmeans(training, centers=8)
closest.cluster <- function(x) {
  cluster.dist <- apply(km$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}

clusters2 <- apply(test, 1, closest.cluster)

results<-cbind(test, as.character(clusters2))


table(Actual=test[,35],Prediction=results[,36])

right<-results[,35]==results[,36]

rate<-sum(right)/length(right)

rate

#without almost all 0.114928
#With only some columns - 0.1336196








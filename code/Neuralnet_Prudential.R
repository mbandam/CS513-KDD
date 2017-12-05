# removing all the objects
rm(list=ls())

# Loading the prudential dataset
LifeInsurance_data <- read.csv("/Users/mounikabandam/Documents/Stevens/CS 513 Knowledge Dis and Data mining/Project/Data/train.csv",na.strings=c("",NA))

# taking only 10000 records to run neuralnetwork because of the performance issue
LifeInsurance_data <- LifeInsurance_data[-c(10001:59381),]

# converting the categorized columns to factor Columns
# class(LifeInsurance_data$Response) #integer
# str(LifeInsurance_data) # as the data in the dataset as num, int... making the categorical values as factors.
# cols <- c("Product_Info_1", "Product_Info_2", "Product_Info_3", "Product_Info_5", "Product_Info_6", "Product_Info_7", "Employment_Info_2", "Employment_Info_3", "Employment_Info_5", "InsuredInfo_1", "InsuredInfo_2", "InsuredInfo_3", "InsuredInfo_4", "InsuredInfo_5", "InsuredInfo_6", "InsuredInfo_7", "Insurance_History_1", "Insurance_History_2", "Insurance_History_3", "Insurance_History_4", "Insurance_History_7", "Insurance_History_8", "Insurance_History_9", "Family_Hist_1", "Medical_History_2", "Medical_History_3", "Medical_History_4", "Medical_History_5", "Medical_History_6", "Medical_History_7", "Medical_History_8", "Medical_History_9", "Medical_History_11", "Medical_History_12", "Medical_History_13", "Medical_History_14", "Medical_History_16", "Medical_History_17", "Medical_History_18", "Medical_History_19", "Medical_History_20", "Medical_History_21", "Medical_History_22", "Medical_History_23", "Medical_History_25", "Medical_History_26", "Medical_History_27", "Medical_History_28", "Medical_History_29", "Medical_History_30", "Medical_History_31", "Medical_History_33", "Medical_History_34", "Medical_History_35", "Medical_History_36", "Medical_History_37", "Medical_History_38", "Medical_History_39", "Medical_History_40", "Medical_History_41","Response")
# LifeInsurance_data[cols] <- lapply(LifeInsurance_data[cols], factor)

# Removing the dummy variables as facing problem with the model running
LifeInsurance_data <- LifeInsurance_data[,-c(80:127)]

# Removng columns which are not required for the model(reason mentioned in DataCleaning file)
LifeInsurance_data <- LifeInsurance_data[,c(-1,-3,-30,-35,-36,-37,-38,-48,-53,-62,-70)]

# code for handling the missing values in traing data
LifeInsurance_data$Employment_Info_1[is.na(LifeInsurance_data$Employment_Info_1)]<-mean(LifeInsurance_data$Employment_Info_1, na.rm = TRUE)

LifeInsurance_data$Employment_Info_4[is.na(LifeInsurance_data$Employment_Info_4)]<-mean(LifeInsurance_data$Employment_Info_4, na.rm = TRUE)

LifeInsurance_data$Medical_History_1[is.na(LifeInsurance_data$Medical_History_1)]<-median(LifeInsurance_data$Medical_History_1, na.rm = TRUE)

LifeInsurance_data$Employment_Info_6[is.na(LifeInsurance_data$Employment_Info_6)]<-mean(LifeInsurance_data$Employment_Info_6, na.rm = TRUE)

# making multi output ANN
one <- ifelse(LifeInsurance_data$Response==1,1,0)
two <- ifelse(LifeInsurance_data$Response==2,1,0)
three <- ifelse(LifeInsurance_data$Response==3,1,0)
four <- ifelse(LifeInsurance_data$Response==4,1,0)
five <- ifelse(LifeInsurance_data$Response==5,1,0)
six <- ifelse(LifeInsurance_data$Response==6,1,0)
seven <- ifelse(LifeInsurance_data$Response==7,1,0)
eight <- ifelse(LifeInsurance_data$Response==8,1,0)
LifeInsurance_data2 <- data.frame(LifeInsurance_data,one,two,three,four,five,six,seven,eight)
# LifeInsurance_data2 <- LifeInsurance_data2[,-c(69)]

# Getting the index values starting form 1 and then every fifth record(1,6,11,16,...)
index<-seq(from=1,to=nrow(LifeInsurance_data2),by=5)

# Splitting the data into traing and test
# Store every fifth record in a "test" dataset starting with the first record
test<-LifeInsurance_data2[index,]
# Store the rest in the "training" dataset
training<-LifeInsurance_data2[-index,]

# install and loading the neuralnet package
# install.packages("neuralnet")
library("neuralnet")

# neural net alogorithm application
n <- names(LifeInsurance_data2)
f <- as.formula(paste("one + two + three + four + five+ six + seven + eight ~", paste(n[!n %in% c("one","two","three","four","five","six","seven","eight","Response")], collapse = " + ")))
# k <- as.formula(paste("~ Response +", paste(n[!n %in% "Response"], collapse = " + ")))
#data_matrix <- model.matrix(k, data = training)
prudentialANN <- neuralnet(f, data = training, hidden=1, threshold=0.01)

# print(prudentialANN)
plot(prudentialANN)
# data_matrix_test <- model.matrix(k, data = test)
prudentialANNresult <- compute(prudentialANN, test[,-c(69:77)] )

# getting the result in another dataframe
prudentialANN_result <- as.data.frame(prudentialANNresult$net.result)
colnames(prudentialANN_result) <- c("one","two","three","four","five","six","seven","eight")
# making the values as signle digit numbers for the calculation of error rate
prudentialANN_result$one<-ifelse(prudentialANN_result$one>=.1,1,0)
prudentialANN_result$two<-ifelse(prudentialANN_result$two>=.1,1,0)
prudentialANN_result$three<-ifelse(prudentialANN_result$three>=.1,1,0)
prudentialANN_result$four<-ifelse(prudentialANN_result$four>=.1,1,0)
prudentialANN_result$five<-ifelse(prudentialANN_result$five>=.1,1,0)
prudentialANN_result$six<-ifelse(prudentialANN_result$six>=.1,1,0)
prudentialANN_result$seven<-ifelse(prudentialANN_result$seven>=.1,1,0)
prudentialANN_result$eight<-ifelse(prudentialANN_result$eight>=.1,1,0)

# neural net is not working for the dataset, this can be cncluded with the following result
prudentialANN_result



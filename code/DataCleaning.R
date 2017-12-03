# removing all the objects
rm(list=ls())

setwd(getwd())

# loading the training data
LifeInsurance_training_data <- read.csv("../Data/train.csv",na.strings=c("",NA))

# loading the test data
LifeInsurance_test_data <- read.csv("../Data/test.csv",na.strings=c("",NA))

# code for handling the missing values in traing data
LifeInsurance_training_data$Employment_Info_1[is.na(LifeInsurance_training_data$Employment_Info_1)]<-mean(LifeInsurance_training_data$Employment_Info_1, na.rm = TRUE)

LifeInsurance_training_data$Employment_Info_4[is.na(LifeInsurance_training_data$Employment_Info_4)]<-mean(LifeInsurance_training_data$Employment_Info_4, na.rm = TRUE)

LifeInsurance_training_data$Employment_Info_6[is.na(LifeInsurance_training_data$Employment_Info_6)]<-mean(LifeInsurance_training_data$Employment_Info_6, na.rm = TRUE)

LifeInsurance_training_data$Insurance_History_5[is.na(LifeInsurance_training_data$Insurance_History_5)]<-mean(LifeInsurance_training_data$Insurance_History_5, na.rm = TRUE)

LifeInsurance_training_data$Family_Hist_2[is.na(LifeInsurance_training_data$Family_Hist_2)]<-mean(LifeInsurance_training_data$Family_Hist_2, na.rm = TRUE)

LifeInsurance_training_data$Family_Hist_3[is.na(LifeInsurance_training_data$Family_Hist_3)]<-mean(LifeInsurance_training_data$Family_Hist_3, na.rm = TRUE)

LifeInsurance_training_data$Family_Hist_4[is.na(LifeInsurance_training_data$Family_Hist_4)]<-mean(LifeInsurance_training_data$Family_Hist_4, na.rm = TRUE)

LifeInsurance_training_data$Family_Hist_5[is.na(LifeInsurance_training_data$Family_Hist_5)]<-mean(LifeInsurance_training_data$Family_Hist_5, na.rm = TRUE)

LifeInsurance_training_data$Medical_History_1[is.na(LifeInsurance_training_data$Medical_History_1)]<-median(LifeInsurance_training_data$Medical_History_1, na.rm = TRUE)

LifeInsurance_training_data$Medical_History_10[is.na(LifeInsurance_training_data$Medical_History_10)]<-median(LifeInsurance_training_data$Medical_History_10, na.rm = TRUE)

LifeInsurance_training_data$Medical_History_15[is.na(LifeInsurance_training_data$Medical_History_15)]<-median(LifeInsurance_training_data$Medical_History_15, na.rm = TRUE)

LifeInsurance_training_data$Medical_History_24[is.na(LifeInsurance_training_data$Medical_History_24)]<-median(LifeInsurance_training_data$Medical_History_24, na.rm = TRUE)

LifeInsurance_training_data$Medical_History_32[is.na(LifeInsurance_training_data$Medical_History_32)]<-median(LifeInsurance_training_data$Medical_History_32, na.rm = TRUE)

# code for handling the missing values in test data
LifeInsurance_test_data$Employment_Info_1[is.na(LifeInsurance_test_data$Employment_Info_1)]<-mean(LifeInsurance_test_data$Employment_Info_1, na.rm = TRUE)

LifeInsurance_test_data$Employment_Info_4[is.na(LifeInsurance_test_data$Employment_Info_4)]<-mean(LifeInsurance_test_data$Employment_Info_4, na.rm = TRUE)

LifeInsurance_test_data$Employment_Info_6[is.na(LifeInsurance_test_data$Employment_Info_6)]<-mean(LifeInsurance_test_data$Employment_Info_6, na.rm = TRUE)

LifeInsurance_test_data$Insurance_History_5[is.na(LifeInsurance_test_data$Insurance_History_5)]<-mean(LifeInsurance_test_data$Insurance_History_5, na.rm = TRUE)

LifeInsurance_test_data$Family_Hist_2[is.na(LifeInsurance_test_data$Family_Hist_2)]<-mean(LifeInsurance_test_data$Family_Hist_2, na.rm = TRUE)

LifeInsurance_test_data$Family_Hist_3[is.na(LifeInsurance_test_data$Family_Hist_3)]<-mean(LifeInsurance_test_data$Family_Hist_3, na.rm = TRUE)

LifeInsurance_test_data$Family_Hist_4[is.na(LifeInsurance_test_data$Family_Hist_4)]<-mean(LifeInsurance_test_data$Family_Hist_4, na.rm = TRUE)

LifeInsurance_test_data$Family_Hist_5[is.na(LifeInsurance_test_data$Family_Hist_5)]<-mean(LifeInsurance_test_data$Family_Hist_5, na.rm = TRUE)

LifeInsurance_test_data$Medical_History_1[is.na(LifeInsurance_test_data$Medical_History_1)]<-median(LifeInsurance_test_data$Medical_History_1, na.rm = TRUE)

LifeInsurance_test_data$Medical_History_10[is.na(LifeInsurance_test_data$Medical_History_10)]<-median(LifeInsurance_test_data$Medical_History_10, na.rm = TRUE)

LifeInsurance_test_data$Medical_History_15[is.na(LifeInsurance_test_data$Medical_History_15)]<-median(LifeInsurance_test_data$Medical_History_15, na.rm = TRUE)

LifeInsurance_test_data$Medical_History_24[is.na(LifeInsurance_test_data$Medical_History_24)]<-median(LifeInsurance_test_data$Medical_History_24, na.rm = TRUE)

LifeInsurance_test_data$Medical_History_32[is.na(LifeInsurance_test_data$Medical_History_32)]<-median(LifeInsurance_test_data$Medical_History_32, na.rm = TRUE)

#Define max-min normalization function
mmnorm <-function(x,minx,maxx) {z<-((x-minx)/(maxx-minx))
return(z) 
}

# the below are the columns which we will not be using while developing a model.
# 1. id - which is not required
# 2. Product_Info_2 - This is an alpha numeric column, we can not convert it into number.
# 3. Insurance_History_5 - The traing dataset has 25396 out of 59831 missing values which is almost 50%, so thought of not using for the model. The test dataset has 8105 out of 19766 missing values.
# 4. Family_Hist_2 - The traing dataset has 28656 out of 59831 missing values which is almost 50%, so thought of not using for the model. The test dataset has 9880 out of 19766 missing values.
# 5. Family_Hist_3 - The traing dataset has 34241 out of 59831 missing values which is more than 50%, so thought of not using for the model. The test dataset has 11064 out of 19766 missing values.
# 6. Family_Hist_4 - The traing dataset has 19184 out of 59831 missing values which is almost 40%, so thought of not using for the model. The test dataset has 6677 out of 19766 missing values.
# 7. Family_Hist_5 - The traing dataset has 41811 out of 59831 missing values which is almost 80%, so thought of not using for the model. The test dataset has 13624 out of 19766 missing values.
# 8. Medical_History_10 - The traing dataset has 58824 out of 59831 missing values which is more than 80%, so thought of not using for the model. The test dataset has 19564 out of 19766 missing values.
# 9. Medical_History_15 - The traing dataset has 44596 out of 59831 missing values which is more than 80%, so thought of not using for the model. The test dataset has 14864 out of 19766 missing values.
# 10. Medical_History_24 - The traing dataset has 55580 out of 59831 missing values which is more than 80%, so thought of not using for the model. The test dataset has 18585 out of 19766 missing values.
# 11. Medical_History_32 - The traing dataset has 58274 out of 59831 missing values which is more than 80%, so thought of not using for the model. The test dataset has 19414 out of 19766 missing values.







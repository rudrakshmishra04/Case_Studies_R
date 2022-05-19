# Installing and loading all the libraries
#Using the pre-processing and EDA steps similar to previous assignemnt
library(polycor)
library(readxl)
library("readxl")
library('C50')
library(rpart)
library("nnet")
library(devtools)
library(reshape)
library(caret)
library(rattle)
library(psych)#categorical correlation
#install.packages('NeuralNetTools')
library(NeuralNetTools)
#install.packages('neuralnet')
library(neuralnet)

df <- read_excel("R://downloads//Patient_Data.xlsx")
#now we inspect data to see each variable
str(df)
#since variable type is char, we change it to factor for all the applicable vairables
df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)],as.factor)
#lets check if they are converted to factors
str(df)
#Lets summarize our data
summary(df)

# to check correlation between each binary categorical data
cc=table(df$diabetes,df$heartattack_s) 
tetrachoric(cc)
cc=table(df$gender,df$heartattack_s) 
tetrachoric(cc)
cc=table(df$smoker,df$heartattack_s) 
tetrachoric(cc)
cc=table(df$active,df$heartattack_s) 
tetrachoric(cc)
cc=table(df$obesity,df$heartattack_s) 
tetrachoric(cc)
cc=table(df$cholesteral,df$heartattack_s) 
tetrachoric(cc)

#EDA
plot(df$heartattack_s,df$age, xlab="heart attack", ylab="patient age")
plot(df$heartattack_s,df$active, xlab="heart attack", ylab="active")
plot(df$heartattack_s,df$smoker, xlab="heart attack", ylab="smoker")
plot(df$heartattack_s,df$gender, xlab="heart attack", ylab="gender")

#outliers in age variable
boxplot(df$age, ylab="age")

#skewness of age variable
hist(df$age,ylab="count",xlab="age")

#checking if the data is balanced 
barplot(table(df$heartattack_s),ylab="count",xlab="heart attack")

#to check if there are any duplicate values
#duplicated(df)
sum(duplicated(df))
nrow(df)
df2 =unique(df)
nrow(df2)

#check for missing values
sum(is.na(df))

##########################################################################################################
h <- preProcess(df[1], method = c("range"))
df3 <- cbind(predict(h, df[,-7]),heartattack_s = df$heartattack_s)
#df3
#building NN with iterations as 1000 and 10 units in hidden layer
model1 <- nnet(heartattack_s ~ ., data = df3, size = 10, maxit = 1000)
plotnet(model1)
View(model1)
#now we calculate the accuracy
pred <- predict(model1, df3[,1:8], type = "class")
mean(pred == df3$heartattack_s)
pred_table = table(predicted = pred, actual = df$heartattack_s)
rec <- pred_tab[2,2]/sum(pred_table[,2])
rec

###########################################################################################################

#Building the c5 model
model2 <- C5.0(heartattack_s ~ ., data=df) 
summary(model2)
plot(model2, type = "simple" , cex =0.7 , main = 'heart attack diagnosis decision tree') 
#calculating the accuracy of our c5 moodel
acc <- predict(model2, df, type = "class")
mean(acc == df$heartattack_s)
pred_table2 = table(predicted = acc, actual = df$heartattack_s)
pred_table2
rec <- pred_table2[2,2]/sum(pred_table2[,2])
rec
########f#####################################################################

#Buliding NN with partition
set.seed(100)
sampl <- createDataPartition(df$heartattack_s, p = 0.80, list = FALSE)
df_train <- df[sampl,]
View(df_train)
df_test <- df[-sampl,]
View(df_test)
heart_partition <- preProcess(df_train[1], method = c("range"))
df_train <- cbind(predict(heart_partition, df_train[-7]),heartattack_s = df_train$heartattack_s)
df_test  <- cbind(predict(heart_partition, df_test[-7]), heartattack_s = df_test$heartattack_s)

model1_partition <- nnet(heartattack_s ~ ., data = df_train, size = 10, maxit = 1000)
plotnet(model1_partition)
#calculating the accuracy and confusion matrix of the train data
pred <- predict(model1_partition, df_train[,1:8], type = "class")
mean(pred == df_train$heartattack_s)

#calculating the accuracy and confusion matrix of the test data
test_predictions <- predict(model1_partition, df_test[,1:8], type = "class")
mean(test_predictions == df_test$heartattack_s)
pred_tab_test = table(predicted = test_predictions, actual = df_test$heartattack_s)
pred_tab_test
rec <- pred_tab_test[2,2]/sum(pred_tab_test[,2])
rec


##trying to increase recall and accuracy
model1_partition <- nnet(heartattack_s ~ ., data = df_train, size = 50, maxit = 1000)
plotnet(model1_partition)
#calculating the accuracy and confusion matrix of the train data
pred <- predict(model1_partition, df_train[,1:8], type = "class")
mean(pred == df_train$heartattack_s)

#calculating the accuracy and confusion matrix of the test data
test_predictions <- predict(model1_partition, df_test[,1:8], type = "class")
mean(test_predictions == df_test$heartattack_s)
pred_tab_test = table(predicted = test_predictions, actual = df_test$heartattack_s)
pred_tab_test
rec <- pred_tab_test[2,2]/sum(pred_tab_test[,2])
rec

model1_partition <- nnet(heartattack_s ~ ., data = df_train, size = 50, maxit = 10000, decay = 0.01)
plotnet(model1_partition)
#calculating the accuracy and confusion matrix of the train data
pred <- predict(model1_partition, df_train[,1:8], type = "class")
mean(pred == df_train$heartattack_s)

#calculating the accuracy and confusion matrix of the test data
test_predictions <- predict(model1_partition, df_test[,1:8], type = "class")
mean(test_predictions == df_test$heartattack_s)
pred_tab_test = table(predicted = test_predictions, actual = df_test$heartattack_s)
pred_tab_test
rec <- pred_tab_test[2,2]/sum(pred_tab_test[,2])
rec



###################### PART-2  ################################################################
library(e1071)
library("klaR")
library("caret")
library(magrittr)
library(dplyr)
library(readxl)

df_c <- read_excel("R:/downloads/default_of_credit_card_clients.xlsx", skip = 1)
str(df_c)
summary(df_c)
head(df_c)
table(df_c$`default payment next month`)

#Removing id 
df_c <- df_c[-1]
#replacing 4/4+ with 4 (others category) in education variable
df_c$EDUCATION[df_c$EDUCATION != c("1","2","3","4")] <- 4
#replacing 3/3+ with 3 (others category) in marriage variable
df_c$MARRIAGE[df_c$MARRIAGE != c("1","2","3")] <- 3
str(df_c)

names(df_c)[names(df_c) == "default payment next month"] <- "defaultpayment"
#factor type convertitions
df_c$SEX <- as.factor(df_c$SEX)
df_c$EDUCATION <- as.factor(df_c$EDUCATION)
df_c$MARRIAGE <- as.factor(df_c$MARRIAGE)
df_c$PAY_0 <- as.factor(df_c$PAY_0)
df_c$PAY_2 <- as.factor(df_c$PAY_2)
df_c$PAY_3 <- as.factor(df_c$PAY_3)
df_c$PAY_4 <- as.factor(df_c$PAY_4)
df_c$PAY_5 <- as.factor(df_c$PAY_5)
df_c$PAY_6 <- as.factor(df_c$PAY_6)
df_c$defaultpayment <- as.factor(df_c$defaultpayment)
str(df_c)
#Checking and getting rid of duplicates
df_c <- df_c[!duplicated(df_c), ]
head(df_c)
str(df_c)


#performing train-test split
set.seed(100)
trainIndex <- createDataPartition(df_c$defaultpayment, p=0.8, list=FALSE)
data_train <- df_c[ trainIndex,]
data_test <- df_c[-trainIndex,]
table(data_train$defaultpayment)
#upsampling the defaultpayments
data_train<-upSample(x=data_train[,-ncol(data_train)],y=data_train$defaultpayment)
names(data_train)[names(data_train) == "Class"] <- "defaultpayment"
table(data_train$defaultpayment)

# build naive bayes model
Naive_Bayes_Model=naiveBayes(defaultpayment ~., data=data_train)
Naive_Bayes_Model

#calculating the accuracy and confusion matrix of the train data
credit_predictions_train <- predict(Naive_Bayes_Model, data_train)
cred_table=table(credit_predictions_train,data_train$defaultpayment)
mean(credit_predictions_train == data_train$defaultpayment)
rec <- cred_table[2,2]/sum(cred_table[,2])

rec
#calculating the accuracy and confusion matrix of the test data
credit_predictions_test <- predict(Naive_Bayes_Model, data_test)
cred_table=table(credit_predictions_test,data_test$defaultpayment)
mean(credit_predictions_test == data_test$defaultpayment)
rec <- cred_table[2,2]/sum(cred_table[,2])
rec 

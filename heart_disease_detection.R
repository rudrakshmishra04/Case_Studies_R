# Installing and loading all the libraries
#install.packages("rattle")
#install.packages("polycor")
#install.packages("dplyr")


library(dplyr)
library(polycor)
library(readxl)
library("readxl")
library('C50')
library(rpart)
library(caret)
library(rattle)
library(psych)#categorical correlation


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


#building the c5.0 model
c50tree1 <- C5.0(df[,-7], as.factor(df$heartattack_s))
#summary 
summary(c50tree1)
#plotting 
plot(c50tree1,type= "simple", cex=.7)
#Now let us compute the accuracy of our model
predictions <- predict(c50tree1, df)
mean(predictions==df$heartattack_s)


#building the cart model
cart <- rpart(heartattack_s~., data = df, method = "class")
summary(cart)
#plotting
fancyRpartPlot(cart, cex = 1.0, caption = "Heart Attack")
Predictcart = predict(cart, data = df, type = "class")
table(df$heartattack_s, Predictcart)
mean(Predictcart==df$heartattack_s)

#Now building c5.0 model with 80:20 partitioning
#train test split with seed
set.seed(100)
df_split <- createDataPartition(df$heartattack_s, p = 0.80, list =FALSE)
df_train <- df[df_split,]
df_train_labels <- df$heartattack_s[df_split]
df_test <- df[-df_split,]
#build model
c50tree3 <- C5.0(df_train[,-7], as.factor(df_train$heartattack_s))
#summary
summary(c50tree3)
#plotting
plot(c50tree3, type="simple")
#Now let us compute the accuracy of our model
train_predictions <- predict(c50tree3, df_train)
mean(train_predictions==df_train$heartattack_s)
test_predictions <- predict(c50tree3, df_test)
mean(test_predictions==df_test$heartattack_s)


## creating c50 with rules= True
c50tree3rules <- C5.0(heartattack_s ~ ., data=df_train, rules = TRUE)
summary(c50tree3rules)
#Now let us compute the accuracy of our model
train_predictions <- predict(c50tree3rules, df_train)
mean(train_predictions==df_train$heartattack_s)
test_predictions <- predict(c50tree3rules, df_test)
mean(test_predictions==df_test$heartattack_s)



### Dealing with the slight imbalance in the data
table(df_train$heartattack_s)
set.seed(100)
trainup<-upSample(x=df_train[,-ncol(df_train)],y=df_train$heartattack_s)
table(trainup$heartattack_s)
# building model for the balanced model
str(trainup)
vars <- c("age", "gender","diabetes","smoker","active","obesity","bp")
c50tree4 <- C5.0(x = trainup[, vars], y = as.factor(trainup$heartattack_s))
#summary
summary(c50tree4)
#plotting
plot(c50tree4, type="simple")
#Now let us compute the accuracy of our model
train_predictions <- predict(c50tree4, trainup)
mean(train_predictions==trainup$heartattack_s)
test_predictions <- predict(c50tree4, df_test)
mean(test_predictions==df_test$heartattack_s)




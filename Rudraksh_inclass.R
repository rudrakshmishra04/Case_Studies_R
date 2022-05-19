
#1
library(e1071)
library("klaR")
library("caret")
library(magrittr)
library(dplyr)
library(utils)
#2
data("Titanic")
df=as.data.frame(Titanic)
head(df)
#3
seq=rep.int(seq_len(nrow(df)), df$Freq)
df=df[seq,]
head(df)
#4
df$Freq=NULL
head(df)
#5
nb=naiveBayes(Survived ~., data=df)
nb
#6
pred=predict(nb,df)
table(pred,df$Survived)
#7
library(mlr)
task = makeClassifTask(data = df, target = "Survived")
mk = makeLearner("classif.naiveBayes")
nb = train(mk, task)
nb$learner.model
#8
pred = as.data.frame(predict(nb, newdata = df[,1:3]))
table(pred[,1],df$Survived)

#9
data("iris")
head(iris)
#10
split=0.80
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]
model <- naiveBayes(Species~., data=data_train)
x_test <- data_test[,1:4]
y_test <- data_test[,5]
#11
predictions <- predict(model, x_test)
confusionMatrix(predictions, y_test)
#12
train <- trainControl(method="boot", number=100)
model <- caret::train(Species~., data=iris, trControl=train, method="nb")
print(model)
#13
train <- trainControl(method="cv", number=10)
model <- caret::train(Species~., data=iris, trControl=train, method="nb")
print(model)


#mushroom data questions
df3 <- read.csv("R:/downloads/mushroom.csv", sep = ",", na.strings = "?", header = TRUE)
df3 = as.data.frame(df3[complete.cases(df3),])
head(df3)
i <- sample(nrow(df3),size = floor(0.7 * nrow(df3)), replace = FALSE)
train <- df3[i,]
test <- df3[-i,]
#17
df3.model <- naiveBayes(train$Class ~ . , data = train, laplace = 1)
print(df3.model)
#18
df3.predict <- predict(df3.model,test, type = 'class')
results <- data.frame(actual = test[,'Class'], predicted = df3.predict)
table(results)


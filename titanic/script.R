rm(list=ls())
data_train = read.csv("C:\\Users\\Abhishek\\Documents\\Programing\\R\\kaggle\\titanic\\train.csv")
attach(data_train)
data_train[!complete.cases(data_train), ]
data_train$Survived = factor(data_train$Survived)
levels(data_train$Survived) 
data_train = na.omit(data_train)# remove na rows
str(data_train)# see data in sting format to remove coluns with more levels
model1 = randomForest(Survived ~. -PassengerId -Name -Cabin -Ticket ,data= data_train , mtry = 3 , importance = TRUE);
model1

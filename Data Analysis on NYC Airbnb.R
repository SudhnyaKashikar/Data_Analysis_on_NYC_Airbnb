#install packages
install.packages("dplyr")
install.packages("tidyverse")
install.packages("scales")
install.packages("arules")
install.packages("arulesViz")
install.packages("gridExtra")
install.packages("factoextra")
install.packages("NbClust")
install.packages("MASS")
install.packages("party")
install.packages("partykit")
install.packages("rpart.plot")

#Importing the data
AB_NYC<-read.csv(file = "~/Desktop/AB_NYC.csv")
View(AB_NYC)

#Hypothesis testing
summary(AB_NYC)
One_sample <- t.test (AB_NYC$availability_365, mu=100, alternative = "greater", conf.level =0.95)
One_sample

#Welch two sample t  test
x<-(AB_NYC$room_type)
y<-(AB_NYC$price)
t.test(x,y)

#Linear regression
library(dplyr)
library(tidyverse)
library(scales)
library(arules) 
library(arulesViz)
library(gridExtra)
#read data
#read file
install.packages("ggplot2")
library(ggplot2)

#i1<-read.csv(file = "~/Desktop/AB_NYC.csv")

library(readr)
i1<- read_csv("excel files/AB_NYC.csv")
View(i1)
# number of rows
nrow(i1)

# number of columns
ncol(i1)


#dropping unique variable and some columns
i1$X=NULL
i1$name=NULL
i1$host_name=NULL
i1$neighbourhood_group=NULL
i1$neighbourhood=NULL
i1$room_type=NULL
i1$id=NULL
i1$host_id=NULL
i1$latitude=NULL
i1$longitude=NULL
i1$last_review=NULL

View(i1)

str(i1)

#Data division
i1_sample= sample(1:nrow(i1), size= floor(nrow(i1)*0.7))
train_data = i1[i1_sample,]
test_data = i1[-i1_sample,]

#building model
model_1 = lm(price~., data= train_data)
summary(model_1)

step_model = step(model_1)

model_2 = lm(price ~ minimum_nights + number_of_reviews + reviews_per_month 
             + calculated_host_listings_count + availability_365 , data = train_data)
summary(model_2)

layout(matrix(c(1,2,3,4),2,2))
plot(model_2)



#Predicting the values with test dataset
train_p = predict(model_2, newdata = train_data)
training = cbind(train_data, train_p)

test_p = predict(model_2, newdata = test_data)
testing = cbind(test_data, test_p)

#checking accuracy of model using corelation
(cor(training$price, training$train_p))
(cor(testing$price, testing$test_p))
 

#Decision tree
#Installing all read librabries and Packages
library(factoextra)
library(NbClust)
library(MASS)
library(party)


#Used for making decision tree
library(partykit)
library(ggplot2)

library(readr)
i1 <-read.csv(file = "~/Desktop/AB_NYC.csv")
View(i1)

#Prepare Data
data<- i1
str(data)
data_frame <- i1[c(2:4)]
data_frame

#Partition of Test and Training Datasets

set.seed(1234)
pd<- sample(2,nrow(data),replace = TRUE,prob = c(0.8,0.2))
train<- data[pd==1,]
test <- data[pd==2,]

treeAnalysis<- rpart(price ~ number_of_reviews + reviews_per_month + availability_365 , data = i1)
treeAnalysis


library(rpart.plot)
rpart.plot(treeAnalysis)

#Prediction
pred1 <- predict(trees,newdata = train)
table(pred1,train$price)

pred <- predict(trees,newdata = test)
table(pred,test$price)




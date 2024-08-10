######SVM Model######
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)


###Code to Import Data
url<-"https://intro-datascience.s3.us-east-2.amazonaws.com/Resort01.csv"
finalProject<-read_csv(url)
finalProject


##Add new column named "RoomChange" which indicates whether a customer's room 
###is different from the one they originally reserved
finalProject$RoomChange<-(finalProject$ReservedRoomType==finalProject$AssignedRoomType)


##Create a dataframe with only needed columns
finalProject.tmodel3<-subset(finalProject, select=c("IsCanceled","LeadTime",
                                                    "CustomerType", "RoomChange", "Country", "MarketSegment"))

##Convert "IsCanceled" column into a gactor
finalProject.tmodel3$IsCanceled<-as.factor(finalProject$IsCanceled)

#Partition dataframe
set.seed(1)
trainList <-
  createDataPartition(y=finalProject.tmodel3$IsCanceled,p=.70,list=FALSE) 
dim(trainList)

#Create a train Set
trainSet<-finalProject.tmodel3[trainList,]

#Create a test set
testSet<-finalProject.tmodel3[-trainList,]

dim(trainSet)
dim(testSet)


#Create SVM Model/Train	a	support	vector
svmModel<-ksvm(IsCanceled~.,data=trainSet, kernel="rbfdot", C=5, cross=3, prob.model=TRUE)
svmModel

#Predict
svmPred<-predict(svmModel,newdata = trainSet)
svmPred

#examine the predicted object with str & calculate the confusion matrix
str(svmPred)
ConfMatrix <-table(svmPred, trainSet$IsCanceled)


#Interpret	the	confusion	matrix and	calculate	the	overall	accuracy	of	the	model.
diag(ConfMatrix)
sum(ConfMatrix)
confusionMatrix(svmPred,trainSet$IsCanceled)

#This is a good model because it has high accuracy and the p-value is less than 0.05.


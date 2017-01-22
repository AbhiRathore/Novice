setwd("D:/kaggle_redhat/Statistics/case1/") # set working directory
library(dplyr)
library(rpart)
library(rpart.plot)
gerber=read.csv("gerber.csv",header = T,na.strings = c("","NA",NA),as.is = T)
head(gerber)
table(gerber$voting) # exercise 1 

# voting", sorted by whether or not the people were in each group:
tapply(gerber$voting, gerber$civicduty, mean)
tapply(gerber$voting, gerber$hawthorne, mean)
tapply(gerber$voting, gerber$self, mean)
tapply(gerber$voting, gerber$neighbors, mean)

LogModel = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial") # without splitting

########################### prediction without split

#First compute predictions:
predictLog = predict(LogModel, type="response")
#Then, use the table function to make a confusion matrix:
table(gerber$voting, predictLog > 0.3)
# We can compute the accuracy of the sum of the true positives and true negatives, divided by the sum of all numbers in the table:
 ## repeat all of these excercises with splitted data , for splitting you can just split with rows take 80% rows in train and 20% in test
 nrow(gerber)
c=.8*nrow(gerber)

 train_gerber=[1:c,]
 test_gerber=[c:nrow(gerber),] # there can be better splits... hint: examlple using caTools sample.split on voting columns 
 
 
 
 
 
 ###########  cart
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
#CARTcontrol = rpart(voting ~ control, data=gerber, cp=0.0) ex. to select only one variable and use of cp
prp(CARTmodel,varlen = 13,extra = 2)


# try prediction by making model on train and testing on test data

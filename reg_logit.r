setwd("D:/kaggle_redhat/Statistics/case1/") # set working directory
library(dplyr)
library(ggplot2)
library(rpart)
# for linear regression
dat_edx=read.csv("climate_change.csv",header = T,na.strings = c("","NA",NA),as.is = T)
head(dat_edx)
summary(dat_edx)
dat_edx$combine=dat_edx$CO2*dat_edx$CH4
plot(dat_edx$Temp~dat_edx$combine)
plot(dat_edx$Temp~(dat_edx$CO2*dat_edx$CH4))
cor(dat)
plot(dat_edx$CO2,dat_edx$CH4,main ="temperature curve",xlab = "combined effect of c02 and ch4", ylab="Temperature")
str(dat_edx)
plot(dat_edx$MEI,dat_edx$Temp)
coplot(dat_edx$Temp~dat_edx$CO2*dat_edx$CH4|dat_edx$Month)
# spliting train and test
train_edx=subset(dat_edx,dat_edx$Year<=2006)
test_edx=subset(dat_edx,dat_edx$Year>2006)
str(train_edx)
str(test_edx)
summary(dat_edx)
rm(text_edx)
model_edx=lm(Temp~.,data=train_edx )
summary(model_edx)
cor(train_edx)
model2_edx=step(model_edx) # getting best model among available models using AIC value
summary(model2_edx)
anova(model2_edx) # anova table
## predict
pred=predict(model2_edx,newdata = test_edx)
head(pred)
SSE = sum((pred- test_edx$Temp)^2)
SST = sum((mean(train_edx$Temp) - test_edx$Temp)^2)
SSM = sum((mean(test_edx$Temp) - pred)^2)
R2 = 1 - SSE/SST
R2

##############
#### logit ( logistics regression)
songs=read.csv("songs.csv",header = T,na.strings = c("",NA,"NA","null"),as.is = T)
#########
str(songs)
head(songs)
summary(songs)
names(songs)
for(i in c(1,2,3,4,5,39)){songs[,i]=as.factor(songs[,i])}
songs$artistname=as.factor(gsub("Various Artists","Various artists",songs$artistname))
table(songs$year)
nrow(songs)
#### split 
songs_train<-subset(songs,!(songs$year %in% c('2008','2008','2009','2010')))
songs_test<-subset(songs,songs$year %in% c('2008','2008','2009','2010'))
#match("key",names(songs))
songs_train$year=as.character.POSIXt(songs_train$year)
songs_test$year=as.character.POSIXt(songs_test$year)
head(songs_train)
table(songs_train$year)
table(songs_test$year)
str(songs_train)
# first logit model
logit1<-glm(Top10~.,data=songs_train[,-c(1:5)],family = "binomial")
plot(logit1)
summary(logit1)
logit2=step(logit1) # using AIC value finding best model
summary(logit2)
library(caret)
log_pred=predict(logit2,newdata = songs_test,type = "response") # predicting values
head(log_pred)
table(songs_test$Top10,log_pred>0.5) # confusion matrix
predict_top10<-data.frame(as.factor(ifelse(log_pred > 0.5,1,0))) 
final=cbind(songs_test,predict_top10)
head(final)
names(final)[names(final) == "as.factor.ifelse.log_pred...0.5..1..0.."] <- 'predict_top10'
write.csv(final,"songs_pred.csv",row.names = F)

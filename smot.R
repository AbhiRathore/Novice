####SMOT
library(DMwR)
library(caTools)
data(iris)
summary(iris)
data<-iris[,c(1,2)]
str(data)
head(data)
colnames(data)[1]<-"len"
colnames(data)[2]<-"wid"
summary(data)
head(data)
#####creating column with less number of 1's
data$target=factor(ifelse(data$len<4.75,1,0))
summary(data)
#### color command using column
plot(data[,1],data[,2],pch=5+as.integer(data[,3]),col=9+as.integer(data[,3]))
head(data)
split<-sample.split(data$target,SplitRatio = 0.8)
dat3=subset(data,split==TRUE)
dat4=subset(data,split==FALSE)
table(data$target)
11/139
table(dat3$target)
table(dat4$target)
2/28
9/111
head(dat3)
nrow(data)
#install.packages("DMwR")
library(DMwR)
###perc.over=percentage oversampling-% produced as minority
##perc.under= % retained as majority
dataSMOTE<-SMOTE(target~.,data,perc.over =250,perc.under = 500)
plot(dataSMOTE[,1],dataSMOTE[,2],pch=6+as.integer(dataSMOTE[,3]),col=1+as.integer(dataSMOTE[,3]))
summary(dataSMOTE)
head(dataSMOTE)        
nrow(dataSMOTE)
table(dataSMOTE$target)
33/39
nrow(data)
######
loandefault<-read.csv(file.choose())
str(loandefault)
head(loandefault)
#split
nrow(loandefault)
table(loandefault$bad_flag)
###
library(caTools)
set.seed(100)
split<-sample.split(loandefault$bad_flag,SplitRatio =0.75)
loandefaultTrain<-subset(loandefault,split==TRUE)
table(loandefaultTrain$bad_flag)
###SMOT
loandefaultTrain$bad_flag<-as.factor(loandefaultTrain$bad_flag)
loandefaultTrainSMOTE=SMOTE(bad_flag~.,loandefaultTrain,perc.over = 500)
loandefaultTrain$bad_flag<-as.numeric(loandefaultTrain$bad_flag)
modelsmote<-glm(bad_flag~.,family=binomial(link='logit'),data=loandefaultTrainSMOTE)
summary(modelsmote)
##
attach(loandefaultTrain)
table(dataSMOTE$target)
dataSMOTE=data.frame(dataSMOTE)
head(dataSMOTE)
splt=sample.split(dataSMOTE$target,SplitRatio = 0.20)
dat2=subset(dataSMOTE,splt==TRUE)
nrow(dat2)
nrow(dataSMOTE)
table(dat2$target)

###
#predict
##test
loandefaultTest<-subset(loandefault,split==FALSE)
table(loandefaultTest$bad_flag)
nrow(loandefaultTest)
##predict
predictTest1<-predict(modelTrain1,newdata=loandefaultTest,type='response')
confusion_matrix<-table(loandefaultTest$bad_flag,predictTest1>0.5)
confusion_matrix
head(predictTest1)
### 
getwd()
loan1=read.csv("test_loan.csv",header = T,as.is = T)
loan_tst=read.csv("test_loan_2.csv",header = T,as.is = T)
total_loan=rbind(loan1,loan_tst)
total_loan$education=as.factor(total_loan$education)
str(total_loan)
table(total_loan$education)
head(loan1)
split=sample.split(total_loan$default,SplitRatio = .8)
tr_loan=subset(total_loan,split==T)
tst_loan=subset(total_loan,split==F)
table(tr_loan$education)
table(tst_loan$education)
 str(loan1)
p="12/SEP/2016"
q="12/aug/2011"
dates <- c("1jan1960", "2jan1960", "31mar1960", "30jul1960")
dates
library(lubridate)
tmp1=as.Date(strptime(p,"%d/%b/%Y"))
tmp2=as.Date(strptime(q,"%d/%b/%Y"))
f=year(tmp1-tmp2)
as.yearmon(strptime(p,"%d/%b/%Y"))-as.yearmon(strptime(q,"%d/%b/%Y"))

library(zoo)
format(tmp1,'%Y')
as.numeric(format(tmp1,'%Y'))*100 + as.numeric(format(tmp1,'%m'))

loan1$default=as.factor(loan1$default)
str(tr_loan)
mdl_1=glm(default~.,data = tr_loan,family = "binomial")
mdl_2=step(mdl_1)
pr2=predict(mdl_1,newdata = loan_tst,type = "response")
levels(tr_loan$education)=levels(loan_tst$education)
table(tr_loan$education)
nrow(tr_loan)
write.csv(tr_loan,"12345.csv",row.names = F)
library(h2o)
h2o.init()
h2o.shutdown()
h2o.init(nthreads = -1)
irisPath = system.file("extdata", "iris.csv", package="h2o")
dat = h2o.importFile(path = irisPath, destination_frame = "iris.hex",header = F)
dat1<-dat[,c(1,2)]
str(dat1)
head(dat1)
colnames(dat1)[1]<-"len"
colnames(dat1)[2]<-"wid"
dat1$target=ifelse(dat1$len<4.75,1,0)
dat1$target= as.factor(dat1$target)
h2o.clusterInfo()
h2o.ceiling(dat1)
summary(dat1)
head(dat1)
glm1=h2o.glm(x=1:2,y=3,training_frame = dat1,seed = -1,family = "binomial")
summary(glm1)

prosPath = system.file("extdata", "prostate.csv", package = "h2o")
prostate.hex = h2o.importFile(path = prosPath)
head(prostate.hex)
prostate.dl = h2o.deeplearning(x = 3:9, y = 2, training_frame = prostate.hex,hidden = c(100, 200), epochs = 5)

prostate.deepfeatures_layer1 = h2o.deepfeatures(prostate.dl, prostate.hex, layer = 1)
prostate.deepfeatures_layer2 = h2o.deepfeatures(prostate.dl, prostate.hex, layer = 2)
head(prostate.deepfeatures_layer1)
head(prostate.deepfeatures_layer2)

gbm1=h2o.gbm(x=1:2,y=3,training_frame = dat1,validation_frame = NULL,distribution = "bernoulli",nfolds = 50)


head(iris)
h2o.glm(Species~.,iris,family = "binomial")
data(iris)
head(iris)
apply(iris[,c(1:3)],2,function(x){sum(is.na(x))})

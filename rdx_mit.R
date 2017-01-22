setwd("D:/kaggle_redhat/Statistics/case1/") # set working directory
library(dplyr)
library(ggplot2)
library(rpart)
dat1=read.csv("D:/kaggle_redhat/Statistics/case1/Newfile.csv",header = T,na.strings = c("",NA,"null","NA"))
head(dat1)
set.seed(20)
tr=dat1[1:c,]
ts=dat1[c:nrow(dat1),]
nrow(tr)
nrow(dat1)
nrow(ts)
c=.8*nrow(dat1)


dat1$ret_1<-ifelse(dat1$Age.in.Years+1==65,1,0)
dat1$ret_2<-ifelse(dat1$Age.in.Years+2==65,1,0)
dat1$ret_3<-ifelse(dat1$Age.in.Years+3==65,1,0)
dat1$ret_4<-ifelse(dat1$Age.in.Years+4==65,1,0)
dat1$ret_5<-ifelse(dat1$Age.in.Years+5==65,1,0)
dat_edx=read.csv("climate_change.csv",header = T,na.strings = c("","NA",NA),as.is = T)
head(dat_edx)
g=select(dat_edx,4:6,Month,7)
head(g)
dat_edx=mutate(dat_edx,impure=CH4*CO2)
head(dat_edx)
byyear=group_by(dat_edx,Year)
head(byyear)
sumYear <- summarize(byyear,count=n())
head(sumYear)

summary(dat_edx)
dat_edx$combine=dat_edx$CO2*dat_edx$CH4
plot(dat_edx$Temp~dat_edx$combine)
plot(dat_edx$Temp~(dat_edx$CO2*dat_edx$CH4))
cor(dat)
plot(dat_edx$CO2,dat_edx$CH4,main ="temperature curve",xlab = "combined effect of c02 and ch4", ylab="Temperature")
str(dat_edx)
plot(dat_edx$MEI,dat_edx$Temp)
coplot(dat_edx$Temp~dat_edx$CO2*dat_edx$CH4|dat_edx$Month)

vote_edx=read.csv("gerber.csv",header = T,na.strings = c("","NA",NA))
head(vote_edx)
str(vote_edx)

table(vote_edx$voting)
 108696/(235388+108696)

LogModel = glm(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, family="binomial")
 
train_edx=subset(dat_edx,dat_edx$Year<=2006)
test_edx=subset(dat_edx,dat_edx$Year>2006)
str(train_edx)
str(test_edx)
summary(dat_edx)
rm(text_edx)
model_edx=lm(Temp~.,data=train_edx )
summary(model_edx)
cor(train_edx)
14/32
model2_edx=step(model_edx)
varImp(model2_edx)
summary(model2_edx)
anova(model2_edx)
## predict
pred=predict(model2_edx,newdata = test_edx)
head(pred)
SSE = sum((pred- test_edx$Temp)^2)
SST = sum((mean(train_edx$Temp) - test_edx$Temp)^2)
SSM = sum((mean(test_edx$Temp) - pred)^2)
SSM+SSE
SSM/SST
SST
SST-SSE
R2 = 1 - SSE/SST
R2
SSE

##############
#### logit
songs=read.csv("songs.csv",header = T,na.strings = c("",NA,"NA","#","null"),as.is = T)
dat_sample=read.csv("TestData2.csv",header = T,na.strings = c("",NA,"NA","#","null"),as.is = T)
nrow(dat_sample)
names(dat_sample)
summary(dat_sample)
View(dat_sample)
ncol(dat_sample)
2 + NA 
#########
str(songs)
head(songs)
summary(songs)
names(songs)
for(i in c(1,2,3,4,5,39)){songs[,i]=as.factor(songs[,i])}
songs$artistname=as.factor(gsub("Various Artists","Various artists",songs$artistname))
#songs$artistname=as.factor(ifelse(songs$artistname =="Various Artists","Various artists",songs$artistname))
table(songs$year)
nrow(songs)
#### split 
#songs_train<-subset(songs,songs$year <= 2007)
#songs_train=filter(songs,year %in% c('2008','2008','2009','2010'))
?mutate
songs_train<-subset(songs,!(songs$year %in% c('2007','2008','2009','2010')))
songs_test<-subset(songs,songs$year %in% c('2007','2008','2009','2010'))
library(dplyr)
#po=select(songs,c(timbre_0_max,key,pitch))
#head(po)
#match("key",names(songs))
memory.limit(size = 128000000)
songs_train$year=as.character.POSIXt(songs_train$year)
songs_test$year=as.character.POSIXt(songs_test$year)
names(songs)
songs_test<-subset(songs,songs$year > 2007)
head(songs_train)
table(songs_train$year)
table(songs_test$year)
str(songs_train)
logit1<-glm(Top10~.,data=songs_train[,-c(1:5)],family = "binomial")
tabl=read.table("new 161.txt",sep = '\t')
names(tabl)
tabl$date=as.POSIXct(tabl$Time)
head(tabl)
head(tabl$Contents)
??glm
head(tabl$Time)
x1="city  celebrated the completion of 30 lakh kilometres which is equivalent to reduction of 600 tonnes of 
carbon emission in the city during its green parade at cubbon park on 12/oct/2016 edwin b  a 42yearold"
x4="city  celebrated the completion of 30 lakh kilometres which is equivalent to reduction of 600 tonnes of 
carbon emission in the city during its green parade at cubbon park on 12/12/2016 edwin b  a 42yearold"
x2="city  celebrated the completion of 30 lakh kilometres which is equivalent to reduction of 600 tonnes of 
carbon emission in the city during its green parade at cubbon park on 12-oct-2016 edwin b  a 42yearold"
x1
y="city  celebrated the completion of 30 lakh kilometres which is equivalent to reduction of 600 tonnes of 
carbon emission in the city during its green parade at cubbon park on Jan 5, 2011 edwin b  a 42yearold"

y1="city  celebrated the completion of 30 lakh kilometres which is equivalent to reduction of 600 tonnes of 
carbon emission in the city during its green parade at cubbon park on edwin b  a 42yearold"
grep('0-9',x,value = T)
date <-"\r\n        Washington,\r\n        Jan 5, 2011"
library(stringr)
rm(x)
str_e
date <-str_extract_all(y,'\\w+\\s\\d+(st)?(nd)?(rd)?(th)?,\\s+\\d+')
date
substr(y,regexpr(pattern = '[^0-9]',text = y,ignore.case = T)[1],regexpr(pattern = '[0-9]',text = y,ignore.case = T)[1]+4)
regexpr(pattern = '\\d+',text = x2,ignore.case = T)[1]
str_extract_all(x4,'\\d+\\/\\w+\\/\\d+')
str_extract_all(x2,'\\d+\\/\\w+\\/\\d+')
str_extract_all(x1,'\\d+\\-\\w+\\-\\d+')[[1]]
str_extract_all(y,'\\w+\\s\\d+(st)?(nd)?(rd)?(th)?,\\s+\\d+')[[1]]
date_ex(y1)
tabl$Contents=as.character(tabl$Contents)
tabl$Contents[1]
tabl[1,8]
date_ex(tabl[1,8])

str(tabl)
date_ex=function(x){
  library(stringr)
  c=str_extract_all(x,'\\d+\\/\\w+\\/\\d+')
  if(c == 'character(0)'){ 
    c1=str_extract_all(x,'\\d+\\-\\w+\\-\\d+')
  if(c1== 'character(0)'){
    c2=str_extract_all(x,'\\w+\\s\\d+(st)?(nd)?(rd)?(th)?,\\s+\\d+')
  if(c2== 'character(0)'){
    print("nul")}
    else print(c2[[1]])
  }
    else print(c1[[1]])
    }
  else print(c[[1]])
}
(tabl$Contents)[1]
names(tabl)


p=date_ex(tabl$Contents)
date_ex()
x2
tabl$dat2=date_ex(tabl$Contents)
str(tabl)
head(tabl$dat2)
data.frame(tabl$dat2)


substr(x,regexpr('\\d+\\/\\w+\\/\\d+',x)[1],regexpr('\\d+\\/\\w+\\/\\d+',x)[1]+10)
substr(x,175,178)
substr
tabl$date2=as.Date(tabl$date,"%Y-%m-%d")
library(coreNLP)
head(tabl$date2)
head(tabl)
plot(logit1)
summary(logit1)
logit2=step(logit1)
summary(logit2)
library(caret)
log_pred=predict(logit2,newdata = songs_test,type = "response")
head(log_pred)
table(songs_test$Top10,log_pred>0.5)
predict_top10<-data.frame(as.factor(ifelse(log_pred > 0.5,1,0)))
final=cbind(songs_test,predict_top10)
ff=cbind(songs_test)
head(final)
names(final)[names(final) == "as.factor.ifelse.log_pred...0.5..1..0.."] <- 'predict_top10'
tpr=22/(153+22)
fpr=17/(1079+17)
plot(fpr,tpr)
plot(roc(songs_test$Top10,log_pred, direction="<"),
     col="blue", lwd=3, main="The ROC Curve")
library(pROC)
??rocr
write.csv2(final,"songs_pred.csv",row.names = F)
(1079+22)/nrow(songs_test)
plot(logit1)

library(rpart)
library(rattle)
library(rpart)
table(songs_train$Top10)
model_cart=rpart(Top10~.,data=songs_train[,-c(1:5)],cp=0.004)
library(rpart.plot)
varImp(model_cart)

nrow(songs_test)
p=prp(model_cart,varlen = 13,extra = 2)
fancyRpartPlot(model_cart)
###########################
head(test_edx)
ret_5=subset(dat1,dat1$ret_5==1)
head(ret_5)
plot(subset(ret_5[,8],table(ret_5$Base.country)>0))
c=subset(ret_5[,8],table(ret_5$Base.country)>0)
table(c)
str(dat1)
c=as.array(table(ret_5$Base.country))
c
barplot(c[,2])
ms=tapply(ret_5$Age.in.Years,ret_5$Base.country,mean,na.rm=T)
ms
class(ms)
library(ggmap)
dat1=read.csv("D:/kaggle_redhat/Statistics/case1/Newfile.csv",header = T,na.strings = c("",NA,"null","NA","#"),as.is = T)
viewed=unique(dat1$Base.country[which(dat1$Base.country !="")])
ll.viewed<- geocode(viewed)
gender_t = tapply(dat1$Age.in.Years, list(as.factor(dat1$Region), as.factor(dat1$Gender)),mean,na.rm=T )
bp <- barplot(gender_t, beside = TRUE, main = "Region wise age analysis of employees", 
              col = c("lightblue", "mistyrose", "lavender","green","red"),
              xlab = "Gender", names = c("Female", "Male"), 
              ylab = "Mean Age", legend = row.names(gender_t), 
              args.legend = list(title = "Region", x = "topright", cex = .7), ylim = c(0, 90))
text(bp, 0, round(gender_t, 1),cex=1,pos=3) 

plot(dat1[,3],dat1[,5],pch=5+as.integer(as.factor(dat1[,2])),col=9+as.integer(as.factor(dat1[,2])),
     xlab="Age",ylab="service in years")
legend("topleft",c("Female", "Male"),pch=5+as.integer(as.factor(dat1[,2])),col=9+as.integer(as.factor(dat1[,2])))

str(dat1)
range(dat1$X2015,na.rm = T)
summary(dat1)
nrow(dat1)
names(dat1)
dat1[,2]=as.factor(dat1[,2])
for (i in c(1,2,4,6,8,9)){
  dat1[,i]=as.factor(dat1[,i])
}
#dat1[is.na(dat1)]<--1
model_reg_1=lm(X2015~.,data=dat1[-c(1,11,12,4,9,6,8)])
summary(model_reg_1)
model_reg_2=step(model_reg_1)
library(data.table)
dat2=as.data.table(dat1)
dat3<-dat2[,":="(Base.country=ifelse(is.na(Base.country)==TRUE,"not known",Base.country))]
head(dat3)
dat3$Base.country=as.factor(dat3$Base.country)
summary(dat3)
str(dat1[,c(2:5)])
head(dat1$X2015,30)
class(c)
text(c[,2],3,cex=1,pos=4) 
hsb2 <- read.table('http://www.ats.ucla.edu/stat/r/faq/hsb2.csv', header=T, sep=",")
attach(hsb2)
head(hsb2)
sesmeans  <- tapply(math, ses, mean,na.rm=T)
barplot(sesmeans, main = "Math by SES", xlab = "SES", ylab = "Mean Math Score", 
        ylim = c(0, 60), names.arg = c("Low", "Mid", "High"))
row.names(c)

barplot(c, main = "retire and location", xlab = "country", ylab = "age", 
        ylim = c(0, 60), names.arg = row.names(c))
attach(dat1)
gender_t = tapply(Age.in.Years, list(as.factor(Region), as.factor(Gender)),mean,na.rm=T )
text(c[,2],0,labels=c[,2],cex=.8)

barplot(t(gender_t), beside = TRUE)
barplot((gender_t), beside = TRUE)
barplot((gender_t), beside = TRUE,args.legend = list(title = "Gender", x = "topright", cex = .7), 
        ylim = c(0, 90))
legend("topleft",row.names(gender_t))
par(mfrow = c(1,1))
femaleses = tapply(math, list(as.factor(ses), as.factor(female)), mean)

barplot(femaleses, beside = TRUE,, main = "Math by SES and gender", 
        col = c("red", "green", "blue"),
        xlab = "Gender", names = c("Male", "Female"), 
        ylab = "Mean Math Score", legend = c("Low", "Medium", "High"), 
        args.legend = list(title = "SES", x = "topright", cex = .7), ylim = c(0, 90))


bp <- barplot(femaleses, beside = TRUE, main = "Math by SES and gender", 
              col = c("lightblue", "mistyrose", "lavender"),
              xlab = "Gender", names = c("Male", "Female"), 
              ylab = "Mean Math Score", legend = c("Low", "Medium", "High"), 
              args.legend = list(title = "SES", x = "topright", cex = .7), ylim = c(0, 90))
text(bp, 0, round(femaleses, 1),cex=1,pos=3) 
bp <- barplot(gender_t, beside = TRUE, main = "Region wise age analysis of employees", 
              col = c("lightblue", "mistyrose", "lavender","green","red"),
              xlab = "Gender", names = c("Female", "Male"), 
              ylab = "Mean Age", legend = row.names(gender_t), 
              args.legend = list(title = "Region", x = "topright", cex = .7), ylim = c(0, 90))
text(bp, 0, round(gender_t, 1),cex=1,pos=3) 

head(dat1)
library(dplyr)
b=barplot(table(as.character(subset(dat1,dat1$ret_5==1)$Base.country)),col=6+as.integer(row.names(o)))
o=data.frame(table(as.character(subset(dat1,dat1$ret_5==1)$Base.country)))
list(o$Var1)

row.names(data.frame(table(as.character(subset(dat1,dat1$ret_5==1)$Base.country))))
text(b, 0, table(as.character(subset(dat1,dat1$ret_5==1)$Base.country)),cex=1,pos=3)
table(filter(dat1,ret_5==1)$Base.country)
head(round(gender_t,1))
names(dat1)
dat1$Base.country=as.character(dat1$Base.country)
dat1$Base.country=as.factor(dat1$Base.country)

f=subset(dat1,dat1$ret_5==1)
head(f)

table(f$Base.country)
str(f)
write.csv(f,"f.csv",row.names = F)
table(f$Base.country)
unique(f$Base.country)
unique(dat1$Base.country)
table(f)
class(f)
legend("topleft",c("Female", "Male"),pch=5+as.integer(dat2[,1]),col=9+as.integer(dat2[,1]))
table(subset(dat1,dat1$Region=="North America")$Gender)

table(as.integer(dat1$Gender))
dat1$yoa<-2016
summary(dat1)
sapply(dat1,function(x){sum(is.na(x))})
plot(dat1[,3],dat1[,5],pch=5+as.integer(dat1[,2]),col=9+as.integer(dat1[,2]),
     xlab="Age",ylab="service in years")
legend("topleft",c("Female", "Male"),pch=5+as.integer(dat2[,1]),col=9+as.integer(dat2[,1]))

nrow(dat1[,3])
head(dat1[,3])
dat2=as.data.frame(dat1[,c(2,3,5)])
head(dat2)
sapply(dat2,function(x){sum(is.na(x))})
str(dat2)
dat2$Age.in.Years=ifelse(is.na(dat2$Age.in.Years)==TRUE,mean(dat2$Age.in.Years,na.rm = T),dat2$Age.in.Years)
dat2$Length.of.Current.service..in.years.=as.numeric(dat2$Length.of.Current.service..in.years.)
plot(dat2[,2],dat2[,3],pch=5+as.integer(dat2[,1]),col=9+as.integer(dat2[,1]),
     xlab="Age",ylab="service in years")

legend("topleft",c("Female", "Male"),pch=5+as.integer(dat2[,1]),col=9+as.integer(dat2[,1]))
nrow(data.frame(dat2[,2]))
head(dat2)
str(data)
table(as.integer(dat2[,1]))
table(dat2[,1])

#######################
df1 <- data.frame(fruit=c("apple", "orange", "xapple", "xorange", 
                          "applexx", "orangexx", "banxana", "appxxle"), group=c("A", "B") )
df1
df1 %>% filter(!grepl("^x|xx$", fruit))
songs<-read.csv("D:/kaggle_redhat/Statistics/case1/songs.csv",header = T,na.strings = c("","NA",NA),as.is = T)
head(songs)
nrow(songs)
ncol(songs)
names(songs)
#install.packages("rworldmap")
library(rworldmap)
6135-700-574
summary(songs)
songs$artistname=as.factor(ifelse(songs$artistname=="Various Artists","Various artists",songs$artistname))
songs$Top10<-as.factor(songs$Top10)
songs$songtitle<-as.factor(songs$songtitle)
songs$songID<-as.factor(songs$songID)
songs$artistID<-as.factor(songs$artistID)

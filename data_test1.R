setwd("D:/kaggle_redhat/Statistics/case1/") # set working directory
test_file=read.csv("testdata_siv_1.csv",header=T,na.strings=c("","NA","null",NA))

##########
head(test_file)
str(test_file)
summary(test_file)
nrow(test_file)
ncol(test_file)
names(test_file)
library(data.table)
library(dplyr)
data.frame(sapply(test_file,function(x){sum(is.na(x))}))
null_data=select(test_file,3,7,9:11,Certificate.Version)
data.frame(sapply(null_data,function(x){sum(is.na(x))}))
c=grep("Date",names(test_file))
names(test_file[,c(9,20,28:30,32:36)])
head(test_file[,c(9,20,28:30,32:36)])
ff=filter(test_file,lag_allot > 20)
table(ff$Region.Name)
head(ff)
l=test_file %>%filter(lag_allot > 30) %>% group_by(Region.Name) %>% summarise(max(lag_allot/30, na.rm = TRUE))
o
sort(l$v2,decreasing = T)
o=data.frame(l[,1],sort(l$v2,decreasing = T))
library(ggplot2)
names(o)<-c("v1","v2")
ggplot(data=o,aes(v1,v2)) + geom_bar(stat = "identity",aes(fill = region,label="v2")) +geom_text(aes(label=round(v2,0)),vjust=-1)+
  ggtitle("time diff in allotment") +xlab("Region") + ylab("Delay")+theme(
  plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold"))



l=as.data.frame(l)
region=as.character(l[,1])
nrow(l)
l
barplot(l)
group
max(1781,744,744)
head(select(ff,Region.Name,lag_allot))

test_file$lag_allot<-dat_diff(test_file$Employment.Start.Date,test_file$Role.Assignment.Date)

library(lubridate)
p="4/1/2011"
q="5/23/1980"
dat_diff(q,p)/365

tmp1=as.Date(p,"%m/%d/%Y")

TMP1=strptime(p,"%m/%d/%Y")
TMP2=strptime(q,"%m/%d/%Y")
as.double(difftime(TMP2,TMP1,units = "days"))

tmp2=as.Date(q,"%m/%d/%Y")
tmp2
f=year(tmp1-tmp2)
as.yearmon(strptime(p,"%d/%b/%Y"))-as.yearmon(strptime(q,"%d/%b/%Y"))

library(zoo)
format(tmp1,'%Y')
d1=as.numeric(format(TMP1,'%Y'))*10000 + as.numeric(format(TMP1,'%m'))*100 +as.numeric(format(TMP1,'%d')) 
d2=as.numeric(format(TMP2,'%Y'))*10000 + as.numeric(format(TMP2,'%m'))*100 +as.numeric(format(TMP2,'%d')) 
diff.POSIXt(tmp1,tmp2)
d2-d1  
rm(TMP2)

dat_diff<-function(x1,x2){
  TMP1=strptime(x1,"%m/%d/%Y")
  TMP2=strptime(x2,"%m/%d/%Y")
  #d1=as.numeric(format(TMP1,'%Y'))*10000 + as.numeric(format(TMP1,'%m'))*100 +as.numeric(format(TMP1,'%d')) 
  #d2=as.numeric(format(TMP2,'%Y'))*10000 + as.numeric(format(TMP2,'%m'))*100 +as.numeric(format(TMP2,'%d'))
  difff=as.double(difftime(TMP2,TMP1,units = "days"))
  return(difff)
}

dat_diff(tmp1,tmp2)  
head(select(ff,Person.Type,Region.Name))
table(ff$Region.Name,ff$Person.Type)



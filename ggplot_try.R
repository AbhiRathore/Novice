setwd("D:/kaggle_redhat")
getwd()
library(gdata)
library(data.table)
trial2=read.xls("D:/crayon_analytics/hdfc/leftout.xlsx",sheet = 1)
install.packages(c("ecm","fsia"))
trial=read.table("clipboard")
library(dplyr)
install.packages("rmarkdown")
getwd()
library(sqldf)
library(maps) 
library(ggmap) 
trn_sc<-fread("train1.csv",sep=",",header = T)
names(trn_sc)
names(trn_sc)[names(trn_sc) == "Destination Station Name"] <- 'Destination'
names(trn_sc)[names(trn_sc) == "source Station Name"] <- 'Source'
names(trn_sc)[names(trn_sc) == "train Name"] <- 'train_Name'
names(trn_sc)[names(trn_sc) == "Train No."] <- 'train_no'
names(trn_sc)[names(trn_sc) == "Station Name"] <- 'st_name'
names(trn_sc)[names(trn_sc) == "Arrival time"] <- 'A_time'
names(trn_sc)[names(trn_sc) == "Departure time"] <- 'D_time'
trn <- trn_sc[,.(train_Name,train_no,Distance,Source,Destination,st_name,A_time,D_time)]
name_list=names(trn_sc)
for x in name_list



head(trn)

r=names(trn_sc)
write.csv(r,"r.csv",row.names = F)
######################
rm(trn)
trn$train_no<-gsub("'","",trn$train_no)
trn <- trn[, ":="(train_Name = as.character(train_Name),
                  train_no = as.numeric(train_no),
                  Distance = as.numeric(Distance),
                  Source = as.character(Source),
                  Destination = as.character(Destination),
                  st_name = as.character(st_name),
                  A_time = gsub("'","",A_time),
                  D_time = gsub("'","",D_time))]

####################
trn$Source <- paste(trn$Source,", INDIA",sep='')   
trn$Destination <- paste(trn$Destination,", INDIA",sep='')
trn$st_name <- paste(trn$st_name,", INDIA",sep='')
##########
###install.packages("rattle")
###install.packages("tree")

tree1<-read.csv(file.choose())
head(tree1)
str(tree1)
head(trn)
fl2=trn[,.(train_Name,Source,new)]
head(fl2)
##########
treedata=read.csv(file.choose())
View(treedata)
library(rattle)
library(RColorBrewer)
set.seed(10)
trn$ran<-runif(nrow(trn),0,1)
trn$new=ifelse(trn$ran<=0.5,"True","FALSE")
library(tree)
treedata$y=as.factor(treedata$y)
str(treedata)
nrow(unique(trn[,.(train_no)]))
nrow(unique(trn[,.(train_Name)]))
head(emp)
plot(trn[,1],trn[,2],
     pch=1+as.integer(trn[,10]),
     col=2+as.integer(trn[,10]),
     main ="Raw data",xlab = "x1", ylab="x2")
plot(emp[,3],emp[,1],
     pch=1+as.integer(emp[,2]),
     col=2+as.integer(emp[,2]),
     main ="employees data",xlab = "years", ylab="calculated status")
table(emp$Calculated.Status)13
table(emp$IdentityType) 3
table(emp$Calculated.Target.Date)14
legend("bottomleft",pch=1+as.integer(emp[,2]), col=2+as.integer(emp[,2]), c("Contractor", "Employee"), bty="o", cex=.8, box.col="darkgreen")
table(emp$gender)
emp1=read.csv("NO.csv",header = T,na.strings = c("na",NA,""))
#emp=read.csv("emp_1.csv",header = T)
emp1$Calculated.Target.Date=as.character(emp1$Calculated.Target.Date)
head(emp1)
emp1=as.data.table(emp1)
table(is.na(emp1$Calculated.Target.Date))
head(emp1)
emp3=emp1[,.(Calculated.Status,IdentityType,Calculated.Target.Date)]

emp4=group_by(emp3,Calculated.Target.Date)
emp_e=filter(emp4,IdentityType=="Employee")
emp_c=filter(emp4,IdentityType=="Contractor")
emp5=summarise(emp_e,identity_type=unique(Calculated.Status),count=n())
head(emp5,10)
emp6=summarise(emp_c,identity_type=unique(Calculated.Status),count=n())
table(emp4$IdentityType)
head(emp4,100)
emp1$Calculated.Target.Date=as.Date(emp1$Calculated.Target.Date,"%d/%m/%Y")
emp4=emp5
library(plotly)
library(ggplot2)
p=ggplot(emp5, aes(Calculated.Target.Date ,identity_type,label=count,fill=count)) +geom_point()+geom_label(color="white",aes(label=count),hjust=0, vjust=1)+
geom_density(position = "stack")+ggtitle("Employee") +xlab("Date") + ylab("Identity type")+theme(
  plot.title = element_text(color="red", size=14, face="bold.italic"),
  axis.title.x = element_text(color="blue", size=14, face="bold"),
  axis.title.y = element_text(color="#993333", size=14, face="bold")
)

p
q=ggplot(emp6, aes(Calculated.Target.Date ,identity_type,label=count,fill=count)) +geom_point()+geom_label(color="white",aes(label=count),hjust=0, vjust=1)+
  geom_density(position = "stack")+ggtitle("Contractor") +xlab("Date") + ylab("Identity type")+theme(
    plot.title = element_text(color="red", size=14, face="bold.italic"),
    axis.title.x = element_text(color="blue", size=14, face="bold"),
    axis.title.y = element_text(color="#993333", size=14, face="bold")
  )

grid.arrange(p,q,ncol=1)

head(emp1)



mtext("Title for Two Plots", outer = TRUE, cex = 1.5)
library(gridExtra)

ggplot(emp6, aes(Source, count_trains,label=rownames(sm[,2]))) +geom_point() +geom_smooth()+geom_text() +scale_size_area()

emp1$Calculated.Target.Date=ifelse(is.na(emp1$Calculated.Target.Date)==TRUE,"unknown",emp1$Calculated.Target.Date)
ggplot(emp5,aes(x=Calculated.Target.Date,y=identity_type,color=count))+geom_point()
ggplotly(t)
p<-ggplot(emp5,aes(Calculated.Target.Date,identity_type,label=count))
p+geom_point()
p+geom_line(color="blue")+geom_text(aes(),hjust=0, vjust=0)
q
png('savings.png')
ggplotly(q)
#p+geom_line(color="blue")+geom_text(aes(),hjust=0, vjust=0)+geom_density()+abline()
dev.off()
plot(emp4[,3],emp[,1])
names(emp1)
plot(emp4[,3],emp4[,1],pch=1+as.integer(emp4[,2]),col=2+as.integer(emp4[,2]),
     main ="employees data",xlab ="years",ylab="calculated status")
head(emp1)
str(emp4[,3])
table(emp4[,2])
"""
legend("topleft", pch=c(2,2), col=c("red", "blue"), c("Male", "Female"), bty="o", cex=.8, box.col="darkgreen")
"""
head(trn[,.(train_Name)])
trn2=as.data.frame(trn[,.(train_Name,train_no,Source)])
head(trn2[,3])
head(ag)
ag=as.data.frame(ag)
write.csv(trn,"testt.csv",row.names = F)
ag=group_by(fl2,Source)
str(ag)
symm=summarise(ag, train = n_distinct(train_Name), count_trains = n())
head(symm)
symm=as.data.frame(symm)
sym22=filter(symm,Source like "%A%")
barplot(symm$Source)
length(unique(trn2[,3]))
plot(treedata[,1],treedata[,2],
     pch=1+as.integer(treedata[,3]),
     col=2+as.integer(treedata[,3]),
     main ="Raw data",xlab = "x1", ylab="x2")
ggplot(symm, aes(Source, flights)) +
  geom_point(aes(size = planes), alpha = 1) +
  geom_smooth() +
  scale_size_area()
head(ag)
nrow(ag)
sm=summarize(ag,train=n_distinct(train_Name),count_trains=n())
head(sm)

ggplot(sm, aes(Source, count_trains,label=rownames(sm[,2]))) +geom_point() +geom_smooth()+geom_text() +scale_size_area()
ggplot(symm, aes(Source, count_trains)) +geom_point(aes(col = symm[,2]), alpha = 1) +geom_smooth() +scale_size_area()
all.stations <- unique(c(trn$Source,trn$Destination,trn$st_name))
#######
all.longitudes <- as.numeric(NA[1:length(all.stations)])  
all.latitudes <- as.numeric(NA[1:length(all.stations)])
co=geocode(location = "600041 Chennai Tamil Nadu  ,Thiruvalluvar Nagar  No. 3,B9/A2, First Main Road, Thiruvalluvar Nagar, 600041")
co
for ( i  in 1: length(all.stations)) 
{ 
  coordinates <- geocode(all.stations[i]) 
  all.longitudes[i] <- coordinates$lon
  all.latitudes[i] <- coordinates$lat
}

all.locations <- as.data.frame(cbind(name=all.stations, 
                                lon=all.longitudes, lat=all.latitudes),
                               stringsAsFactors=FALSE) 
all.locations$lon <- as.numeric(all.locations$lon)                                                                          
all.locations$lat <- as.numeric(all.locations$lat)
no.of.trains <- as.data.frame(trn[,c("Source","Destination")] 
                              %>% group_by(Source,Destination) %>% 
                                summarise(count=n()))
class(all.locations[,2])='int32'
options(digits=6)
head(all.locations)
head(trn)
trn$source_lat<-NULL
trn$source_lat<-match(trn$Source,all.locations,as.numeric(all.locations$lat))
class(trn[,9])='int32'
options(digits=6)
head(trn)
write.csv(all.locations,"geo.csv",row.names = F)
write.csv(trn,"trn.csv",row.names = F)
which.min(c(4,1,6))
                        
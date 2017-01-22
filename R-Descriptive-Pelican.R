setwd("F:/PGPM-Pandyas/R-CSV")
mydata=read.csv("PelicanStores.csv", header=TRUE)
mydata
attach(mydata)
#Overall Summary
summary(mydata)
#CrossTab between Gender and Marital Status)
table(Gender, MaritalStatus)
#CrossTab between CustomerType and PaymentMethod
table(CustomerType, PaymentMethod)
#Individual Summary of NetSales for MaritalStatus, CustomerType, Gender, and PaymentMethod
data1=by(NetSales, INDICES=MaritalStatus, FUN=summary)
data2=by(NetSales, INDICES=CustomerType, FUN=summary) 
data3=by(NetSales, INDICES=Gender, FUN=summary)
data4=by(NetSales, INDICES=PaymentMethod, FUN=summary)
data1
data2
data3
data4
#Boxplots ordinary type for Individual Categories(CustomerType, PaymentMethod, MaritalStatus, and Gender)
boxplot(NetSales~CustomerType, col=c("Green", "Pink"), ylab="Net Sales", main="Comparative Sales by Customer Type")
boxplot(NetSales~PaymentMethod, col=c("Green", "Pink", "Red", "Orange", "Blue"), ylab="Net Sales", main="Comparative Sales by Payment Method")
boxplot(NetSales~MaritalStatus, col=c("Green", "Pink"), main="Comparative Sales by Marital Status", ylab="Net Sales")
boxplot(NetSales~Gender, col=c("Green", "Pink"), main="Comparative Sales by Gender", ylab="Net Sales")
#Scatter plots and Boxplots using ggplot2 for aesthetic effects
library(ggplot2)
Scatter=ggplot(mydata, aes(Items, NetSales))
Scatter+geom_point(color="Orange")+geom_smooth(method="lm", color="Red")
Scatter+geom_point(color="Orange")+geom_smooth(method="lm", color="Red")+labs(title="Relationship between Net Sales and Items")+labs(x="Items", y="Net Sales")
Scatter1=ggplot(mydata, aes(Items, NetSales), color=MaritalStatus)
Scatter1+geom_point(color="Orange")+geom_smooth(method="lm", aes(fill=MaritalStatus), color="Red")+labs(title="Relationship between Net Sales and Items")+labs(x="Items", y="Net Sales")
Scatter2=ggplot(mydata, aes(Items, NetSales), color=Gender)
Scatter2+geom_point(color="Orange")+geom_smooth(method="lm", aes(fill=Gender), color="Red")+labs(title="Relationship between Net Sales and Items")+labs(x="Items", y="Net Sales")
Boxplot=ggplot(mydata, aes(CustomerType, NetSales))
Boxplot+geom_boxplot(aes(fill=CustomerType))+labs(x="Customer Type", y="Net Sales")


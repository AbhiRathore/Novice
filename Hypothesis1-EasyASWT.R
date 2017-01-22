setwd("F:/R for SMDM/R-csv")
mydata=read.csv("golf.csv", header=TRUE)
mydata
Current=mydata$Current
New=mydata$New
tstat1=t.test(Current, New)
tstat1
tstat2=t.test(Current, New, var.equal=TRUE)
tstat2

 
 

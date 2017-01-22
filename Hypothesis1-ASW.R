setwd("F:/R for SMDM/R-csv")
data1=read.csv("Quality.csv", header=TRUE)
data1
Sample1=data1$Sample1
Sample2=data1$Sample2
Sample3=data1$Sample3
Sample4=data1$Sample4
x1bar=mean(Sample1)
x2bar=mean(Sample2)
x3bar=mean(Sample4)
x3bar=mean(Sample3)
x4bar=mean(Sample4)
#Null Hypothesis is Mu=12
#Alternative Hypothesis is Mu not =12
n=30
sigma=0.21
Mu=12
z1=(x1bar-Mu)/(sigma/sqrt(n))
z2=(x2bar-Mu)/(sigma/sqrt(n))
z3=(x3bar-Mu)/(sigma/sqrt(n))
z4=(x4bar-Mu)/(sigma/sqrt(n))
computedz=c(z1, z2, z3, z4) 
computedz
alpha=0.01
criticalz=qnorm(1-alpha/2)
criticalz
pvaluez1=2*(1-pnorm(abs(z1)))
pvaluez2=2*(1-pnorm(abs(z2)))
pvaluez3=2*(1-pnorm(abs(z3)))
pvaluez4=2*(1-pnorm(abs(z4)))
pvalue=c(pvaluez1, pvaluez2, pvaluez3, pvaluez4)
pvalue
sd1=sd(Sample1)
sd2=sd(Sample2)
sd3=sd(Sample3)
sd4=sd(Sample4)
SampleSds=c(sd1,sd2,sd3,sd4)
SampleSds
confidenceL1=mean(Sample1)-sd(Sample1)/sqrt(n)*criticalz
confidenceU1=mean(Sample1)+sd(Sample1)/sqrt(n)*criticalz
confidence1=c(confidenceL1, confidenceU1)
confidence1
 



setwd("F:/R for SMDM/R-CSV")
mydata= read.csv("Judge.csv", header=TRUE)
mydata
attach(mydata)
#Each  Court Probability As a Whole
data1=mydata[1:16, ]
names(data1)=c("Judge1", "Disposed1","Appealed1", "Reversed1", "Court1")
attach(data1)
PrAppeal1=sum(Appealed1)/sum(Disposed1)
PrReversed1=sum(Reversed1)/sum(Disposed1)
PrReversed1GivenAppeal1=sum(Reversed1)/sum(Appealed1)
OverallCommon=data.frame(PrAppeal1, PrReversed1, PrReversed1GivenAppeal1)
names(OverallCommon)=c("PrAppeal", "PrReversed", "PrReversedGivenAppeal")
OverallCommon
data2=mydata[17:20, ]
names(data2)=c("Judge2", "Disposed2","Appealed2", "Reversed2", "Court2")
attach(data2)
PrAppeal2=sum(Appealed2)/sum(Disposed2)
PrReversed2=sum(Reversed2)/sum(Disposed2)
PrReversed2GivenAppeal2=sum(Reversed2)/sum(Appealed2)
OverallDomestic=data.frame(PrAppeal2, PrReversed2, PrReversed2GivenAppeal2)
names(OverallDomestic)=c("PrAppeal", "PrReversed", "PrReversedGivenAppeal")
OverallDomestic
data3=mydata[21:40, ]
names(data3)=c("Judge3", "Disposed3","Appealed3", "Reversed3", "Court3")
attach(data3)
PrAppeal3=sum(Appealed3)/sum(Disposed3)
PrReversed3=sum(Reversed3)/sum(Disposed3)
PrReversed3GivenAppeal3=sum(Reversed3)/sum(Appealed3)
OverallMunicipal=data.frame(PrAppeal3, PrReversed3, PrReversed3GivenAppeal3)
names(OverallMunicipal)=c("PrAppeal", "PrReversed", "PrReversedGivenAppeal")
OverallMunicipal
#Comparison of Performance among Courts
Comparison=matrix(c(OverallCommon, OverallDomestic, OverallMunicipal), ncol=3)
Comparison
#Individual Judge Ranking-Common Court
data4=mydata[1:16,]
attach(data4)
PrAppeal=Appealed/Disposed
PrAppeal
PrReversal=Reversed/Disposed
PrReversal
PrReversalGivenAppeal=Reversed/Appealed
PrReversalGivenAppeal
Results=data.frame(Judge, Court, PrAppeal, PrReversal, PrReversalGivenAppeal)
Results
ResultsRankCommon=data.frame(Judge, Court, rank(PrAppeal), rank(PrReversal), rank(PrReversalGivenAppeal))
ResultsRankCommon
#Individual Judge Ranking-Domestic Court
data5=mydata[17:20,]
attach(data5)
PrAppeal=Appealed/Disposed
PrAppeal
PrReversal=Reversed/Disposed
PrReversal
PrReversalGivenAppeal=Reversed/Appealed
PrReversalGivenAppeal
Results=data.frame(Judge, Court, PrAppeal, PrReversal, PrReversalGivenAppeal)
Results
ResultsRankDomestic=data.frame(Judge, Court, rank(PrAppeal), rank(PrReversal), rank(PrReversalGivenAppeal))
ResultsRankDomestic
#Individual Judge Ranking-Municipal Court
data6=mydata[21:40,]
attach(data6)
PrAppeal=Appealed/Disposed
PrAppeal
PrReversal=Reversed/Disposed
PrReversal
PrReversalGivenAppeal=Reversed/Appealed
PrReversalGivenAppeal
Results=data.frame(Judge, Court, PrAppeal, PrReversal, PrReversalGivenAppeal)
Results
ResultsRankMunicipal=data.frame(Judge, Court, rank(PrAppeal), rank(PrReversal), rank(PrReversalGivenAppeal))
ResultsRankMunicipal

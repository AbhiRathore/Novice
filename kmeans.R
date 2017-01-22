setwd("D:/kaggle_redhat/Statistics/case1/") # set working directory
dailykos = read.csv("dailykos.csv")
kosDist = dist(dailykos, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")
head(kosDist)
plot(kosHierClust)
rect.hclust(kosHierClust, k = 5, border = "red")
data(USArrests)
head(USArrests)
kosDist = dist(USArrests, method="euclidean")
kosHierClust = hclust(kosDist, method="ward.D")



hierGroups = cutree(kosHierClust, k = 7)
head(HierCluster1)
#Then, you can use the subset function 7 times to split the data into the 7 clusters:
  HierCluster1 = subset(dailykos, hierGroups == 1)
HierCluster2 = subset(dailykos, hierGroups == 2)
HierCluster3 = subset(dailykos, hierGroups == 3)
HierCluster4 = subset(dailykos, hierGroups == 4)
HierCluster5 = subset(dailykos, hierGroups == 5)
HierCluster6 = subset(dailykos, hierGroups == 6)
HierCluster7 = subset(dailykos, hierGroups == 7)

names(HierCluster1)
tail(sort(colMeans(HierCluster1)))
tail(sort(colMeans(HierCluster2)))
row.names(USArrests)
###Hierarchical Clustering
#Now repeat the command given in the previous problem for
#each of the other clusters, and answer the following questions.
#Which words best describe cluster 2?


november, vote, edward, bush
kerry, bush, elect, poll
november, poll, vote, challenge
bush, democrat, republican, state
unanswered
Which cluster could best be described as the cluster related to the Iraq war?


Cluster 1
Cluster 2
Cluster 3
Cluster 4
Cluster 5
Cluster 6
Cluster 7
unanswered
In 2004, one of the candidates for the Democratic nomination for the President of the United States was Howard Dean, John Kerry was the candidate who won the democratic nomination, and John Edwards with the running mate of John Kerry (the Vice President nominee). Given this information, which cluster best corresponds to the democratic party?


Cluster 1
Cluster 2
Cluster 3
Cluster 4
Cluster 5
Cluster 6
Cluster 7
unanswered
Explanation
You can repeat the command on each of the clusters by typing the following:
  tail(sort(colMeans(HierCluster2)))
tail(sort(colMeans(HierCluster3)))
tail(sort(colMeans(HierCluster4)))
tail(sort(colMeans(HierCluster5)))
tail(sort(colMeans(HierCluster6)))
tail(sort(colMeans(HierCluster7)))
''' 
You can see that the words that best describe Cluster 2 are november, poll, vote, and challenge. The most common words in Cluster 5 are bush, iraq, war, and administration, so it is the cluster that can best be described as corresponding to the Iraq war. And the most common words in Cluster 7 are dean, kerry, poll, and edward, so it looks like the democratic cluster.

Show Answer
SubmitSubmit Your Answer You have used 0 of 2 attempts
Problem 2.1 - K-Means Clustering
3 points possible (graded)
Now, run k-means clustering, setting the seed to 1000 right before you run the kmeans function. Again, pick the number of clusters equal to 7. You don't need to add the iters.max argument.

Subset your data into the 7 clusters (7 new datasets) by using the "cluster" variable of your kmeans output.

How many observations are in Cluster 3?
'''
'''
set.seed(1000)
KmeansCluster = kmeans(dailykos, centers=7)
KmeansCluster = kmeans(USArrests, centers=7)

#Then, you can subset your data into the 7 clusters by using the following commands:
KmeansCluster1 = subset(dailykos, KmeansCluster$cluster == 1)
KmeansCluster2 = subset(dailykos, KmeansCluster$cluster == 2)
KmeansCluster3 = subset(dailykos, KmeansCluster$cluster == 3)
KmeansCluster4 = subset(dailykos, KmeansCluster$cluster == 4)
KmeansCluster5 = subset(dailykos, KmeansCluster$cluster == 5)
KmeansCluster6 = subset(dailykos, KmeansCluster$cluster == 6)
KmeansCluster7 = subset(dailykos, KmeansCluster$cluster == 7)

Alternatively, you could answer these questions by looking at the output of table(KmeansCluster$cluster).
More Advanced Approach:
There is a very useful function in R called the "split" function. Given a vector assigning groups like KmeansCluster$cluster, you could split dailykos into the clusters by typing:
KmeansCluster = split(dailykos, KmeansCluster$cluster)
head(KmeansCluster)
Then cluster 1 can be accessed by typing KmeansCluster[[1]], cluster 2 can be accessed by typing 
KmeansCluster[[2]], etc. If you have a variable in your current R session called "split", 
you will need to remove it with rm(split) before using the split function.
rm(list=ls())
library(ggplot2)
library(cluster)    
library(purrr)
library(dplyr)
library(ISLR)
library(factoextra)
data(USArrests)
str(USArrests)

summary(USArrests)
any(is.na(USArrests))

datascaled <- scale(USArrests)
head(datascaled)

# Dissimilarity matrix
d <- dist(datascaled, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Compute with agnes
hc2 <- agnes(datascaled, metric = "euclidean", method = "complete")
hc2

# Agglomerative coefficient
hc2$ac

# methods 
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(datascaled, method = x)$ac
}

map_dbl(m, ac)

hc3 <- agnes(datascaled, method = "ward")
pltree(hc3, cex = 0.6, hang = -1) 
# compute divisive hierarchical clustering
hc4 <- diana(datascaled)

# Divise coefficient; amount of clustering structure found
hc4$dc

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1)

hc5 <- hclust(d, method = "ward.D2")

library(factoextra)
gap_stat <- clusGap(datascaled, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

sub_grp <- cutree(hc5, k = 4)
sub_grp

# Cut agnes() tree into 4 groups

cutree(as.hclust(hc3), k = 4)

# Cut diana() tree into 4 groups
cutree(as.hclust(hc4), k = 4)

table(sub_grp)

datanew <- cbind(USArrests, cluster = sub_grp)
head(datanew)

fviz_cluster(list(data =USArrests , cluster = sub_grp))

#find mean values for each cluster
aggregate(datanew,by=list(cluster=datanew$cluster), mean)

rm(list=ls())
library(ggplot2)
library(cluster)
library(stats)
library(factoextra)
data(USArrests)
str(USArrests)
any(is.na(USArrests))
datascaled <- scale(USArrests)
head(datascaled)

#optimal number based on Elbow method
fviz_nbclust(datascaled, kmeans, method = "wss")
#optimal number based on gap statistic
gap_stat <- clusGap(datascaled,FUN = kmeans,nstart = 25,K.max = 10,B = 50)
fviz_gap_stat(gap_stat)

set.seed(1)
km <- kmeans(datascaled, centers = 4, nstart=25)
km

fviz_cluster(km, data =USArrests)
datanewkmeans <- cbind(USArrests, cluster = km$cluster)
head(datanewkmeans)
aggregate(USArrests, by=list(cluster=km$cluster), mean)

library(NbClust)
library(cluster)

nclust1<-NbClust(datascaled,distance="euclidean",min.nc=2,max.nc=15,method="kmeans")
nclust2<-NbClust(datascaled,distance="euclidean",min.nc=2,max.nc=15,method="ward.D2")

library(clValid)
library(cluster)
clmethods <- c("hierarchical","kmeans","diana","agnes")
validationmeasures<-c("internal", "stability")

result<- clValid(datascaled, nClust = 2:4,
clMethods = clmethods, validation = validationmeasures, metric="euclidean",method="ward")
# Summary
summary(result)

rm(list=ls())
library(ggplot2)
library(cluster)    
library(purrr)
library(dplyr)
library(ISLR)
library(factoextra)
library(rattle)
data(wine)
str(wine)

true_label <- wine$Type
wine$Class <- NULL
str(wine)

summary(wine)
any(is.na(wine))

datascaled <- scale(wine)
head(datascaled)

# methods 
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(datascaled, method = x)$ac
}

map_dbl(m, ac)

hc<- agnes(datascaled, method = "ward")
pltree(hc, cex = 0.6, hang = -1) 

d <- dist(datascaled, method = "euclidean")
hc2 <- hclust(d, method = "ward.D2")

gap_stat <- clusGap(datascaled, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

sub_grp <- cutree(hc2, k = 3)
sub_grp

table(sub_grp)

datanew <- cbind(wine, cluster = sub_grp)
head(datanew)

#find mean values for each cluster
aggregate(datanew,by=list(cluster=datanew$cluster), mean)

table(datanew$cluster,true_label)

rm(list=ls())
library(ggplot2)
library(cluster)
library(stats)
library(factoextra)
library(rattle)
data(wine)
str(wine)
any(is.na(wine))
true_label <- wine$Type
wine$Type <- NULL
datascaled <- scale(wine)
head(datascaled)

#optimal number based on Elbow method
fviz_nbclust(datascaled, kmeans, method = "wss")
#optimal number based on gap statistic
gap_stat <- clusGap(datascaled,FUN = kmeans,nstart = 25,K.max = 10,B = 50)
fviz_gap_stat(gap_stat)
gap_stat

set.seed(1)
km <- kmeans(datascaled, centers = 3, nstart=25)
km

fviz_cluster(km, data =wine)
datanewkmeans <- cbind(wine, cluster = km$cluster)
head(datanewkmeans)
aggregate(wine, by=list(cluster=km$cluster), mean)

table(datanewkmeans$cluster,true_label)






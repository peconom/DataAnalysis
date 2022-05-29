library(psych)

#get the data
datafilename="http://personality-project.org/R/datasets/extraversion.items.txt" 
mydata=read.table(datafilename,header=TRUE)  
str(mydata)
alpha<-psych::alpha(mydata[,2:6],check.keys=TRUE)
alpha
mydata.keys.list <- list(Extraversion=c(1, -2, 3, -4,5))
mydata.keys <- make.keys(mydata[,2:6],mydata.keys.list,item.labels=colnames(mydata))
mydata.scored <-scoreItems(mydata.keys, mydata[,2:6], impute="NULL", min=1, max=6, digits=3)

library(psychTools)
library(psych)
str(epi)

data("epi.dictionary")
epi.dictionary
epi.keys

library(psychTools)
library(psych)
str(epi)

data("epi.dictionary")
epi.dictionary
epi.keys
Lie.dat<-data.frame(epi[,c(6,24,36,12,18,30,42,48,54)])
alphaLie<-psych::alpha(Lie.dat,check.keys=TRUE)
alphaLie


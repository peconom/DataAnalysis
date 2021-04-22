
library(psychTools)
library(psych)
str(epi)

data("epi.dictionary")
epi.dictionary
epi.keys
Lie.dat<-data.frame(epi[,c(6,24,36,12,18,30,42,48,54)])
alphaLie<-psych::alpha(Lie.dat,check.keys=TRUE)
alphaLie
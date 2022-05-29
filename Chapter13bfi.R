rm(list=ls())
library(psych)
str(bfi)
?bfi

Agreeableness.dat<-data.frame(bfi[,1:5])
Conscientiousness.dat<-data.frame(bfi[,6:10])
Extraversion.dat<-data.frame(bfi[,11:15])
EmotionalStability.dat<-data.frame(bfi[,16:20])
Openness.dat<-data.frame(bfi[,21:25])

psych::alpha(Agreeableness.dat)
psych::alpha(Conscientiousness.dat)
psych::alpha(Extraversion.dat)
psych::alpha(EmotionalStability.dat)
psych::alpha(Openness.dat)

alphaAgree<-psych::alpha(Agreeableness.dat,check.keys=TRUE)
alphaConcie<-psych::alpha(Conscientiousness.dat,check.keys=TRUE)
alphaExtraversion<-psych::alpha(Extraversion.dat,check.keys=TRUE)
alphaEmotionalSt<-psych::alpha(EmotionalStability.dat,check.keys=TRUE)
alphaOpenness<-psych::alpha(Openness.dat,check.keys=TRUE)
alphaAgree
alphaConcie
alphaExtraversion
alphaEmotionalSt
alphaOpenness


scale.names <- c("Agreeableness", "Conscientiousness", "Extraversion","Emotional Stability", "Openness")
alphaCronbach <- as.numeric(c(alphaAgree$total[1],
                              alphaConcie$total[1],
                              alphaExtraversion$total[1],
                              alphaEmotionalSt$total[1],
                              alphaOpenness$total[1]))
alphaCronbachtable <- data.frame(Scale = scale.names, Raw_Alpha = alphaCronbach)
alphaCronbachtable


alphaCronbach2 <- matrix(c(alphaAgree$total[7:8],
                               alphaConcie$total[7:8],
                               alphaExtraversion$total[7:8],
                               alphaEmotionalSt$total[7:8],
                               alphaOpenness$total[7:8]),ncol=2,byrow =TRUE)

alphaCronbachtable2 <- data.frame(Scale = scale.names, Mean = as.numeric(alphaCronbach2[,1]), Sd=as.numeric(alphaCronbach2[,2]))
alphaCronbachtable2

alphaAgree$total
alphaAgree$alpha.drop 
alphaAgree$item.stats 

bfi.keys.list <- list(Agreeableness=c(-1, 2, 3, 4, 5),
                      Conscientiousness=c(6, 7, 8, -9, -10),
                      Extraversion=c(-11, -12, 13, 14, 15),
                      EmotionalStability=c(16, 17, 18, 19, 20),
                      Openness=c(21, -22, 23, 24, -25))
bfi.keys <- make.keys(bfi[,1:25],bfi.keys.list,item.labels=colnames(bfi))
bfi.scored <-scoreItems(bfi.keys, bfi[,1:25], impute="NULL",min=1, max=6, digits=3)

bfi.scored$alpha
bfi.scored$av.r

bfi.scored$cor
datascores<-bfi.scored$scores
datascores

datanew1<- cbind(bfi,datascores)

newdata<-bfi
newdata[,c(1, 9,10,11,12,22,25)] <-7-newdata[,c(1, 9,10,11,12,22,25)] 

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


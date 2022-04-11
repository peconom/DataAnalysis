rm(list=ls())
library(randtests)
data<-data(sweetpotato)
str(sweetpotato)
#runs test of randomness with normal approximation
result1<-runs.test(sweetpotato$production,"two.sided",threshold=median(sweetpotato$production),pvalue="normal", plot=TRUE)
#Cox and Stuart test of randomness
result2<-cox.stuart.test(sweetpotato$production,"two.sided")
# Turning point test of randomness
result3<-turning.point.test(sweetpotato$production,"two.sided")
#Wallis and Moore 1943 test of randomness
result4<-difference.sign.test(sweetpotato$production,"two.sided")
# Mann-Kendal Rank test of randomness
result5<-rank.test(sweetpotato$production,"two.sided")
#Bartels rank test of randomness with Beta approximation since 10<n<100
result6<-bartels.rank.test(sweetpotato$production,"two.sided",pvalue="beta")
result1
result2
result3
result4
result5
result6

rm(list=ls())
library(datasets)
data<-data("iris")
str(iris)

library(moments)
petalwidth<- iris$Petal.Width
summary(petalwidth)
moments::skewness(petalwidth)
moments::kurtosis(petalwidth)


hist(petalwidth)
# fitted normal density
f.den <- function(t) dnorm(t,mean(petalwidth),sqrt(var(petalwidth)))
curve(f.den,xlim=c(0,3))
hist(petalwidth,prob=T,add=T)
par (mfrow=c (1 ,2) )
boxplot (petalwidth, xlab ="  " , ylab =" Petal Width " )


library(e1071)
# p-plot: you should observe a good fit of the straight line

probplot(petalwidth, qdist=qnorm)

# qq-plot: you should observe a good fit of the straight line

qqnorm(petalwidth)
qqline(petalwidth)

library(normtest)
library(nortest)
# Adjusted Jarque–Bera test for normality
#The p-value is computed by Monte Carlo simulation 10000 runs
ajb.norm.test(petalwidth,10000)

shapiro.test(petalwidth)
ad.test(petalwidth)


rm(list=ls())
library(datasets)
library(ggplot2)

data <- datasets::faithful

str(faithful)

eruptionsdata<-data$eruptions

#summary of the data in order to check the minimum and maximum value
summary(eruptionsdata)
#second step histogram

hist(eruptionsdata,xlab = "eruptions", main = "Histogram of Eruptions")
# third step box-plot

boxplot(eruptionsdata,ylab = "Eruptions")
# detect the observations

boxplot(eruptionsdata,ylab = "eruptions",main = "Boxplot of Eruptions")
mtext(paste("Outliers: ", paste(boxplot.stats(eruptionsdata)$out, collapse = ", ")))
out_ind1 <- which(eruptionsdata %in% boxplot.stats(eruptionsdata)$out)
out_ind1
#percentiles
outlier_ind2 <- which(eruptionsdata < quantile(eruptionsdata, 0.025)| eruptionsdata > quantile(eruptionsdata, 0.975))
outlier_ind2

#interval

outlier_ind3 <- which(eruptionsdata < (mean(eruptionsdata)-3*sd(eruptionsdata)) | eruptionsdata > (mean(eruptionsdata)+3*sd(eruptionsdata)))
outlier_ind3

#Hampel filter

outlier_ind4 <- which(eruptionsdata < (mean(eruptionsdata)-3*mad(eruptionsdata,constant=1)) | eruptionsdata > (mean(eruptionsdata)+3*mad(eruptionsdata,constant=1)))
outlier_ind4


library(outliers)
scoresz<-scores(eruptionsdata,"z")
outlier_ind5 <- which(abs(scoresz)>1.96)
outlier_ind5
length(outlier_ind5)/length(scoresz)

outlier_ind6 <- which(abs(scoresz)>2.58)
outlier_ind6
length(outlier_ind6)/length(scoresz)

outlier_ind7 <- which(abs(scoresz)>3.29)
outlier_ind7
length(outlier_ind7)/length(scoresz)



#Rosner test

library(EnvStats)
rosnertest <- rosnerTest(eruptionsdata,k = 1)
rosnertest




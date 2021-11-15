rm(list=ls())
library(randtests)
data(sweetpotato)
str(sweetpotato)

#runs test of randomness with normal approximation
result1<-runs.test(sweetpotato$yield,"two.sided",threshold=median(sweetpotato$yield),pvalue="normal", plot=TRUE)
result1

#Cox and Stuart test of randomness
result2<-cox.stuart.test(sweetpotato$yield,"two.sided")
# Turning point test of randomness
result3<-turning.point.test(sweetpotato$yield,"two.sided")
#Wallis and Moore 1943 test of randomness
result4<-difference.sign.test(sweetpotato$yield,"two.sided")
# Mann-K\endal Rank test of randomness
result5<-rank.test(sweetpotato$yield,"two.sided")
#Bartels rank test of randomness with Beta approximation since 10<n<100
result6<-bartels.rank.test(sweetpotato$yield,"two.sided",pvalue="beta")

rm(list=ls())
library(datasets)
data<-data("iris")
str(iris)

library(moments)
sepallength <- iris$Sepal.Length
summary(sepallength)
moments::skewness(sepallength)
moments::kurtosis(sepallength)

hist(sepallength)
# fitted normal density
f.den <- function(t) dnorm(t,mean(sepallength),sqrt(var(sepallength)))
curve(f.den,xlim=c(4,8))
hist(sepallength,prob=T,add=T)
par (mfrow=c (1 ,2) )
boxplot (sepallength, xlab ="  " , ylab =" Sepal Length " )

library(e1071)
# p-plot: you should observe a good fit of the straight line

probplot(sepallength, qdist=qnorm)

# qq-plot: you should observe a good fit of the straight line
qqnorm(sepallength)
qqline(sepallength)

library(normtest)
# Adjusted Jarque–Bera test for normality
#The p-value is computed by Monte Carlo simulation 10000 runs
ajb.norm.test(sepallength,10000)
 
library(PoweR)

#Shapiro-Wilk statistic
statcompute(21, sepallength, levels = c(0.05, 0.01))
#Anderson-Darling 
statcompute(2, sepallength, levels = c(0.05, 0.01))
# Jarque-Bera
statcompute(7, sepallength, levels = c(0.05, 0.01))

rm(list=ls())
library(ggplot2)
data <- ggplot2::mpg
str(mpg)
hwydata<-data$hwy
#summary of the data in order to check the minimum and maximum value
summary(hwydata)

#second step histogram
hist(hwydata,xlab = "hwy", main = "Histogram of Çighway miles per gallon")
# third step box-plot
boxplot(hwydata,ylab = "hwy",main = "Boxplot of highway miles per gallon")

mtext(paste("Outliers: ", paste(boxplot.stats(hwydata)$out, collapse = ", ")))
out_ind1 <- which(hwydata %in% boxplot.stats(hwydata)$out)
out_ind1

possible1<-data$hwy[213]
possible2<-data$hwy[222]
possible3<-data$hwy[223]
possible1
possible2
possible3

#percentiles
outlier_ind2 <- which(hwydata < quantile(hwydata, 0.025)| hwydata > quantile(hwydata, 0.975))
outlier_ind2

outlier_ind3 <- which(hwydata < (mean(hwydata)-3*sd(hwydata)) | hwydata > (mean(hwydata)+3*sd(hwydata)))
outlier_ind3
#Hampel filter
outlier_ind4 <- which(hwydata < (mean(hwydata)-3*mad(hwydata,constant=1)) | hwydata > (mean(hwydata)+3*mad(hwydata,constant=1)))
outlier_ind4

library(outliers)
scoresz<-scores(hwydata,"z")
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
rosnertest <- rosnerTest(hwydata,k = 3)
rosnertest

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
# Mann-K\endal Rank test of randomness
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


library(PoweR)

#Shapiro-Wilk statistic
statcompute(21, petalwidth, levels = c(0.05, 0.01))
#Anderson-Darling 
statcompute(2, petalwidth, levels = c(0.05, 0.01))
# Jarque-Bera
statcompute(7, petalwidth, levels = c(0.05, 0.01))

library(normtest)
# Adjusted Jarque–Bera test for normality
#The p-value is computed by Monte Carlo simulation 10000 runs
ajb.norm.test(petalwidth,10000)


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






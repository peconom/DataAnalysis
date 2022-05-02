rm(list=ls())
library(datasets)
data<-data("iris")
str(iris)

library(moments)
sepallength <- iris$Sepal.Length
summary(sepallength)
moments::skewness(sepallength)
moments::kurtosis(sepallength)


hist(sepallength,prob=T,ylim=c(0,0.5))
# fitted normal density
f.den <- function(t) dnorm(t,mean(sepallength),sqrt(var(sepallength)))
curve(f.den,xlim=c(4,8),add=T)
boxplot (sepallength, xlab ="  " , ylab =" Sepal Length " )

library(rcompanion)
plotNormalHistogram( sepallength, prob = FALSE,
                      main = "Normal Distribution overlay on Histogram",
                      length = 1000 )


hist(sepallength, freq = FALSE)
curve(dnorm(x, mean = mean(sepallength), sd =sd(sepallength)),
      from = 3,
      to = 9,
      add = TRUE,
     col = "red")


library(e1071)
# p-plot: you should observe a good fit of the straight line

probplot(sepallength, qdist=qnorm)

# qq-plot: you should observe a good fit of the straight line
qqnorm(sepallength)
qqline(sepallength)


library(normtest)
library(nortest)
# Adjusted Jarque–Bera test for normality
#The p-value is computed by Monte Carlo simulation 10000 runs
ajb.norm.test(sepallength,10000)

shapiro.test(sepallength)
ad.test(sepallength)







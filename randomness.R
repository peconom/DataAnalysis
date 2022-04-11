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
# Mann-Kendal Rank test of randomness
result5<-rank.test(sweetpotato$yield,"two.sided")
#Bartels rank test of randomness with Beta approximation since 10<n<100
result6<-bartels.rank.test(sweetpotato$yield,"two.sided",pvalue="beta")
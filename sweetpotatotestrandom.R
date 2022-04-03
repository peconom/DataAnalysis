rm(list=ls())
library(randtests)
data(sweetpotato)
str(sweetpotato)

#runs test of randomness with normal approximation
runs.test(sweetpotato$yield,"two.sided",threshold=median(sweetpotato$yield),pvalue="normal", plot=TRUE)


#Cox and Stuart test of randomness
cox.stuart.test(sweetpotato$yield,"two.sided")
# Turning point test of randomness
turning.point.test(sweetpotato$yield,"two.sided")
#Wallis and Moore 1943 test of randomness
difference.sign.test(sweetpotato$yield,"two.sided")
# Mann-Kendal Rank test of randomness
rank.test(sweetpotato$yield,"two.sided")
#Bartels rank test of randomness with Beta approximation since 10<n<100
bartels.rank.test(sweetpotato$yield,"two.sided",pvalue="beta")
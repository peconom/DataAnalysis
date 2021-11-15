library(psych)
library(dplyr)
str(iris)
data("iris")
iris

data=iris[,-5]
coriris=cor(data)
simplycoriris=round(coriris,3)
simplycoriris

pcanalysis<-principal(coriris,nfactors=length(data),rotate="none")
pcanalysis

eigenvpca<-eigen(cor(data))
eigenv

data1 <- data %>% mutate_all(~(scale(.) %>% as.vector))
a=rbind(c(0.89, 0.36, -0.28, -0.04),c(-0.46, 0.88,  0.09,  0.02), c(0.99, 0.02,  0.05,  0.12),c(0.96,0.06,  0.24, -0.08))

pcascores=data1*a

plot(pcanalysis$values,type="b")

pcanalysis1<-principal(coriris,nfactors=2,rotate="none")
pcanalysis1

rm()
library(psych)
library(ppcor)
library(dplyr)
str(bfi)
data("bfi")
bfi

data=bfi[1:25]
dim(data) # retrieve the dimension of the dataset
describe(data)

cormatrix<-cor(data,use = "complete.obs") # find the correlations using complete observations
round(cormatrix,3) # print correlation rounded to 3 decimal places
lowerCor(data) # print the lower part of the correlation matrix
lowerMat(partial.r(data)) # table of partial correlations
det(cormatrix) #determinant
cortest.bartlett(data) #Bartlett's test
KMO(data) # KMO and MSA

famodel1<-fa(data, nfactors=25, rotate="none")
famodel1
scree(data)
fa.parallel(data)
my.vss <- VSS(data)
my.vss

famodel2<-principal(data, nfactors=5, rotate="none", scores=TRUE)
famodel2
print.psych(famodel2,cut=0.4,sort=TRUE)
famodel3<- principal(data, nfactors=5, rotate="varimax", scores=TRUE)
famodel3
print.psych(famodel3,cut=0.4,sort=TRUE)
fa.diagram(famodel3)

famodel3$scores
head(famodel3$scores,20)
datanew<- cbind(data,famodel3$scores)

rm()
library(psych)
library(dplyr)
str(bfi)
data("bfi")
bfi
data=bfi[1:25]
describe(data)
cormatrix<-cor(data,use = "complete.obs") # find the correlations
round(cormatrix,3) # print correlation rounded to 3 decimal places
lowerCor(data)
lowerMat(partial.r(data)) # table of partial correlations
det(cormatrix) #determinant
cortest.bartlett(data) #Bartlett's test
KMO(data) # KMO and MSA
famodel1<-fa(data, nfactors=25, rotate="none")
famodel1
scree(data)
fa.parallel(data)
my.vss <- VSS(data)
my.vss
famodel4<-fa(data, nfactors=5, fm="minres", rotate="varimax", scores=TRUE)
famodel4
print.psych(famodel4,cut=0.3,sort=TRUE)
fa.diagram(famodel4)
famodel4$scores
head(famodel4$scores,20)
datanew1<- cbind(data,famodel4$scores)

det(Harman23.cor$cov) #determinant
cortest.bartlett(Harman23.cor$cov, n=305) #Bartlett's test
KMO(Harman23.cor$cov) # KMO and MSA
famodel1<-fa(Harman23.cor$cov, nfactors=8, rotate="none")

library(psych)
library(dplyr)
str(USJudgeRatings)
data("USJudgeRatings")
USJudgeRatings

corUSJudge=cor(USJudgeRatings)
simplycorUSJudge=round(corUSJudge,3)
simplycorUSJudge

data=USJudgeRatings[,-1]
cormatrix=cor(data)
cormatrixsimply=round(cor(data),3)
cormatrixsimply

pcanalysis<-principal(cormatrix,nfactors=length(data),rotate="none")
pcanalysis
eigenvpca<-eigen(cor(data))
eigenvpca

plot(pcanalysis$values,type="b")

pcanalysis1<-principal(cormatrix,nfactors=2,rotate="none")
pcanalysis1

rm()
library(psych)
library(psychTools)
library(dplyr)
str(msq)
data("msq")
msq
data=msq[,c("active", "energetic", "vigorous", "wakeful", "wide.awake", "full.of.pep","lively", "sleepy", "tired", "drowsy","intense", "jittery", "fearful", "tense", "clutched.up", "quiet", "still", "placid", "calm", "at.rest")]
describe(data)

cormatrix<-cor(data,use = "complete.obs") # find the correlations
round(cormatrix,3) # print correlation rounded to 3 decimal places
lowerCor(data)
lowerMat(partial.r(data)) # table of partial correlations
det(cormatrix) #determinant
cortest.bartlett(data) #Bartlett's test
KMO(data) # KMO and MSA

famodel1<-fa(data, nfactors=20, rotate="none")
famodel1
scree(data)
fa.parallel(data)
my.vss <- VSS(data)
my.vss

famodel4<-fa(data, nfactors=4, fm="minres", rotate="varimax", scores=TRUE)
famodel4
print.psych(famodel4,cut=0.3,sort=TRUE)
fa.diagram(famodel4)
famodel4$scores
head(famodel4$scores,20)
datanew1<- cbind(data,famodel4$scores)




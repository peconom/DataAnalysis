#Example 4.1
#----------------------------------------------------------------

A <- c(6.33, 6.28, 6.5, 6.40, 6.45)
B <- c(6.51, 6.55, 6.43, 6.51, 6.62, 6.40, 6.48)
sample.na <- length(A)
sample.nb <- length(B)
population.sda <- sqrt(0.008)
population.sdb <- sqrt(0.005)
sample.meanerrora <- population.sda/sqrt(sample.na)
sample.meanerrorb <- population.sdb /sqrt(sample.nb)
alpha = 0.05
z.score = qnorm(p=alpha/2,lower.tail=F)
sample.meana <-mean(A)
sample.meanb <- mean(B)
lower.bounda <- sample.meana - z.score * sample.meanerrora
upper.bounda <- sample.meana +  z.score * sample.meanerrora

lower.boundb <- sample.meanb - z.score * sample.meanerrorb
upper.boundb <- sample.meanb +z.score * sample.meanerrorb

cat("CI for A = (",lower.bounda,",", upper.bounda, ")\n")
cat("CI for B = (",lower.boundb,",",upper.boundb, ")\n")


library(asbio)
ci.mu.z(A, conf = 0.95, sigma = sqrt(0.008))
ci.mu.z(B, conf = 0.95, sigma = sqrt(0.005))

ci.mu.z(conf=0.95,sigma=sqrt(0.005),summarized=TRUE,xbar=6.5, n=7)


#Example 4.2
#----------------------------------------------------------------

A <- c(6.33, 6.28, 6.5, 6.40, 6.45)
B <- c(6.51, 6.55, 6.43, 6.51, 6.62, 6.40, 6.48)
sample.na <- length(A)
sample.nb <- length(B)
sample.sda <- sd(A)
sample.sdb <- sd(B)
sample.sea <- sample.sda/sqrt(sample.na)
sample.seb <- sample.sdb/sqrt(sample.nb)
alpha = 0.05
degrees.freedoma = sample.na - 1
degrees.freedomb = sample.nb - 1
t.scorea = qt(p=alpha/2, df=degrees.freedoma,lower.tail=F)
t.scoreb = qt(p=alpha/2, df=degrees.freedomb,lower.tail=F)
sample.meana <-mean(A)
sample.meanb <- mean(B)
lower.bounda <- sample.meana - t.scorea * sample.sea
upper.bounda <- sample.meana +  t.scorea * sample.sea

lower.boundb <- sample.meanb - t.scoreb * sample.seb
upper.boundb <- sample.meanb +  t.scoreb * sample.seb

cat("CI for A = (",lower.bounda,",", upper.bounda, ")\n")
cat("CI for B = (",lower.boundb,",",upper.boundb, ")\n")

t.test(A, conf.level = 0.95)$conf.int
t.test(B, conf.level = 0.95)$conf.int

ci.mu.t(A, conf = 0.95)
ci.mu.t(B, conf = 0.95)
ci.mu.t(conf=0.95,summarized=TRUE,xbar=6.5, sd=0.07348469, n=7)

#bootstrap CI for the mean
library(confintr)
data<- c(6.33, 6.28, 6.5, 6.40, 6.45)
ci_mean(data,type = "bootstrap")


#Example 4.3
#----------------------------------------------------------------

A <- c(6.33, 6.28, 6.5, 6.40, 6.45)
B <- c(6.51, 6.55, 6.43, 6.51, 6.62, 6.40, 6.48)
sample.na <- length(A)
sample.nb <- length(B)
population.vara <- 0.008
population.varb <- 0.005
var.meanerrora <- population.vara/sample.na
var.meanerrorb <- population.varb /sample.nb
alpha = 0.01
z.score = qnorm(p=alpha/2,lower.tail=F)
sample.meana <-mean(A)
sample.meanb <- mean(B)
lower.bound <- (sample.meana- sample.meanb)- z.score * sqrt(var.meanerrora+var.meanerrorb)
upper.bound <- (sample.meana- sample.meanb)+ z.score * sqrt(var.meanerrora+var.meanerrorb)

cat("CI for mean A-mean B = (",lower.bound,",", upper.bound, ")\n")

#Example 4.4
#----------------------------------------------------------------
A <- c(6.33, 6.28, 6.5, 6.40, 6.45)
B <- c(6.51, 6.55, 6.43, 6.51, 6.62, 6.40, 6.48)
t.test(A,B, paired = FALSE, var.equal=TRUE,conf.level = 0.99)$conf.int

t.test(A,B, paired = FALSE, var.equal=FALSE,conf.level = 0.99)$conf.int


#bootstrap 
library(confintr)
data1<- c(6.33, 6.28, 6.5, 6.40, 6.45)
data2 <- c(6.51, 6.55, 6.43, 6.51, 6.62, 6.40, 6.48)
ci_mean_diff(data1, data2, type = "bootstrap", R  = 9999)

#Example 4.5
#----------------------------------------------------------------
data1<- c(326.5,326.6,326.6,326.8,326.3,326.6,326.7,326.7,326.3)
data2 <- c(326.5,326.6,326.5,326.7,326.3,326.5,326.7,326.6,326.2)
t.test(data1, data2, paired = TRUE)$conf.int

#bootstrap

library(confintr)
data1<- c(326.5,326.6,326.6,326.8,326.3,326.6,326.7,326.7,326.3)
data2 <- c(326.5,326.6,326.5,326.7,326.3,326.5,326.7,326.6,326.2)
diff<- data1-data2
ci_mean(diff,type = "bootstrap")

#Example 4.6
#----------------------------------------------------------------

library(Hmisc)
binconf(45, 100, alpha=0.05, method=c("all"))

library(binom)
binom.confint(45, 100, alpha=0.05,methods=c("exact","wilson","asymptotic"))

prop.test(45,100, p = NULL, correct=FALSE)$conf.int

#Example 4.7
#----------------------------------------------------------------
library(DescTools)
## wald / simple asymptotic interval
BinomDiffCI(26, 62, 24, 85, conf.level = 0.95, method ="wald")
## wald / simple asymptotic interval with continuity correction
BinomDiffCI(26, 62, 24, 85, conf.level = 0.95, method ="waldcc")

x <- c(26, 24)
total <- c(62,85)
prop.test(x, total)$conf.int

#subsection 4.2.6
#----------------------------------------------------------------
var.interval = function(data, conf.level ) {df = length(data) - 1
chilower = qchisq((1 - conf.level)/2, df)
chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
v = var(data)
c(lower=df * v/chiupper, upper=df * v/chilower)
}
data<-rnorm(50)
var.interval(data,0.95)

library(Ecfun)
confint.var(var(data),length(data)-1,level=0.95)
confint.sd(sd(data),length(data)-1,level=0.95)

#subsection 4.2.7
#----------------------------------------------------------------
var.test(A, B, ratio = 1, alternative = c("two.sided"), conf.level = 0.95)

#Example 4.10
#----------------------------------------------------------------
A <- c(6.33, 6.28, 6.5, 6.40, 6.45)
t.test(A, alternative = c("two.sided"), mu = 6.4)

B <- c(6.51, 6.55, 6.43, 6.51, 6.62, 6.40, 6.48)
t.test(B, alternative = c("greater"), mu = 6.4)

#Example 4.11
#----------------------------------------------------------------
A <- c(6.33, 6.28, 6.5, 6.40, 6.45)
B <- c(6.51, 6.55, 6.43, 6.51, 6.62, 6.40, 6.48)
t.test(A, B, alternative = c("less"), mu = 0,  var.equal = TRUE)

#Example 4.12
#----------------------------------------------------------------
data1<- c(326.5,326.6,326.6,326.8,326.3,326.6,326.7,326.7,326.3)
data2 <- c(326.5,326.6,326.5,326.7,326.3,326.5,326.7,326.6,326.2)
t.test(data1, data2, paired = TRUE)

#subsection 4.3.4
#----------------------------------------------------------------
varTest(x, alternative = "two.sided", conf.level = 0.95, 
        sigma.squared = sigma02, data.name = NULL)


#Example 4.13
#----------------------------------------------------------------
library(EnvStats)
B <- c(6.51, 6.55, 6.43, 6.51, 6.62, 6.40, 6.48)
varTest(B, alternative = "greater", conf.level = 0.95,sigma.squared = 0.0025, data.name = NULL)

#Example 4.14
#----------------------------------------------------------------
library(EnvStats)
A <- c(6.33, 6.28, 6.5, 6.40, 6.45)
B <- c(6.51, 6.55, 6.43, 6.51, 6.62, 6.40, 6.48)
var.test(A, B, ratio = 1, alternative = c("two.sided"),
         conf.level = 0.90)

#Example 4.15
#----------------------------------------------------------------
prop.test(45,100,0.5, correct = TRUE)

#Example 4.16
#----------------------------------------------------------------
prop.test(c(4, 12), c(110, 150), alternative =  "less", correct = FALSE)

#section 4.4

wilcox.test(x, y,alternative = "two.sided",mu = 0, paired = FALSE)

wilcox.test(x, y,alternative = "two.sided",mu = 0, paired = FALSE,correct = TRUE,conf.level=0.95 )

wilcox.test(x, y, paired = TRUE, alternative = "two.sided")

#Example 4.18
#----------------------------------------------------------------
obs <- c(18, 22, 30, 21, 17,12)
prob <-c(1/6, 1/6, 1/6, 1/6,1/6,1/6)
chisq.test(obs, p = prob)


#Example 4.19
#----------------------------------------------------------------
obs<- c(2291, 1631, 282, 79, 325,332, 48, 12)
prob<- c(0.373, 0.385, 0.0627, 0.028, 0.071, 0.073, 0.0013, 0.006)

chisq.test(obs, p=prob)

#Example 4.20
#----------------------------------------------------------------
mPau <-matrix(c(2291, 1631, 282, 79, 325,332, 48, 12), nrow =2, ncol=4, byrow =T)
chisq.test(mPau)



#Section 4.6
#----------------------------------------------------------------
library(HSAUR)
data("BtheB",package = "HSAUR")
str(BtheB)

summary(BtheB)


data_for_boxplot <- reshape::melt(data = BtheB[,c("bdi.pre","bdi.2m")])
qplot(x = variable,y = value,data = data_for_boxplot,geom = "boxplot",fill =variable)



chisq.test(BtheB$length, BtheB$drug)
chisq.test(BtheB$treatment, BtheB$drug)
chisq.test(BtheB$treatment, BtheB$length)

table(BtheB$treatment, BtheB$drug)

t.test(formula=bdi.pre~treatment, data=BtheB,var.equal=F)

data_only_TAU <- BtheB[BtheB$treatment=="TAU",]
t.test(x = data_only_TAU$bdi.pre, y = data_only_TAU$bdi.2m,paired = T,alternative = "greater")

data_only_BtheB <- BtheB[BtheB$treatment=="BtheB",]
t.test(x = data_only_BtheB$bdi.pre, y = data_only_BtheB$bdi.2m,paired = T, ,alternative = "greater")


t.test(formula=bdi.2m~treatment, data=BtheB,var.equal=F,alternative = "greater")

#Autoaxiologisi 4.1
#----------------------------------------------------------------
notHealthy<-c(37.2, 41.7,32.1, 38.3, 40.5, 39.4, 53.2, 40.8, 43.7, 45.4)
Healthy<-c(32.7, 36.4, 39.3, 42.5, 38.4, 27.9, 30.1, 33.7, 37.2, 41.8)
t.test(notHealthy,con.level=0.95)$conf.int # (1)
t.test(Healthy,con.level=0.95)$conf.int    # (2)
t.test(notHealthy, Healthy, paired = FALSE, var.equal=TRUE, con.level=0.95)  # (3)

#E#Autoaxiologisi 4.2
#----------------------------------------------------------------
L1<-c(17, 16, 21, 14, 18, 24, 16, 14, 21, 23, 13, 18)
L2<-c(18, 14, 19, 11, 23, 21, 10, 13, 19, 24, 15, 20)
t.test(L1, L2, paired = TRUE,  con.level=0.95)

##Autoaxiologisi 4.3
#----------------------------------------------------------------
prop.test(27,371, 1/20, alternative = "greater", correct = FALSE)

#Autoaxiologisi 4.4
#----------------------------------------------------------------
mST <-matrix(c(136-7,7,55-9,9,239-24,24), nrow=3, ncol=2,byrow=T)
chisq.test(mST)



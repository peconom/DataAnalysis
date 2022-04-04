library(datarium)
library(e1071)
library(MASS)
library(car)
library(mfp)
library(GGally)
library(ggplot2)
library(olsrr)
data("marketing", package = "datarium")

summary(marketing)

pairs(marketing)

GGally::ggpairs(marketing, progress=F)

lm1<-lm(sales~youtube+facebook+newspaper,data=marketing)
summary(lm1)
confint(lm1)
round(vcov(lm1),8)

predict.at=data.frame(youtube=200,facebook=28,newspaper=40) 
predict(lm1,newdata=predict.at,interval="confidence",level=0.95)
predict(lm1,newdata=predict.at,interval="prediction",level=0.95)

lm2<-lm(sales~youtube+facebook,data=marketing)
summary(lm2)
c(AIC(lm1),AIC(lm2)) 

fit_meas <- ols_step_all_possible(lm1)
fit_meas
cbind(fit_meas$aic,fit_meas$sbc)


lmnull <- lm(sales ~ 1, data=marketing)
stepAIC(lmnull,direction="both",scope=list(upper=lm1,lower=lmnull))


lmnull <- lm(sales ~ 1, data=marketing)
stepAIC(lmnull,direction="both",scope=list(upper=lm1,lower=lmnull))
par(mai=rep(0.5, 4))
layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE))
lm2.stdres = rstandard(lm2) 
qqPlot(lm2.stdres,main="Normal Q-Q Plot")

plot(lm2.stdres, pch = 16, col = "black",main="Stand. Residuals versus order", ylab="Stand. residuals",xlab="Order")
abline(0,0)
plot(fitted(lm2), lm2.stdres, col = "black", pch = 20,
     xlab = "Fitted", ylab = "Stand. Residuals", main = "Stand. Residuals versus Fitted")
abline(0,0)



shapiro.test(lm2.stdres)

durbinWatsonTest(lm2)

par(mfrow=c(2, 1))

plot(marketing$youtube, lm2.stdres, col = "black", pch = 20,
     xlab = "Fitted", ylab = "Stand. Residuals", main = "Stand. Residuals versus youtube")
abline(0,0)

plot(marketing$facebook, lm2.stdres, col = "black", pch = 20,
     xlab = "Fitted", ylab = "Stand. Residuals", main = "Stand. Residuals versus facebook")
abline(0,0)


par(mfrow=c(2, 1))
plot(cooks.distance(lm2), pch = 16, col = "blue", main = "Cook's distances",  ylab="Di",xlab="Order",ylim=c(0,max(1.1,cooks.distance(lm2)))) 
abline(1,0)
plot(hatvalues(lm2), pch = 16, col = "blue", main = "Diagonal elements of the hat matrix", ylab="hii",xlab="Order") 
abline(2*mean(hatvalues(lm2)),0)

marketing[which(hatvalues(lm2)>2*mean(hatvalues(lm2))),]



salesstar<-(marketing$sales/(sqrt((marketing$facebook-28)^2+1)))
youtubestar<-I(marketing$youtube/(sqrt((marketing$facebook-28)^2+1)))
facebookstar<-I(marketing$facebook/(sqrt((marketing$facebook-28)^2+1)))

lmw<-lm(salesstar~youtubestar+facebookstar)
summary(lmw)

par(mai=rep(0.5, 4))
layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE))
lmw.stdres = rstandard(lmw) 
qqPlot(lmw.stdres,main="Normal Q-Q Plot")

plot(lmw.stdres, pch = 16, col = "black",main="Stand. Residuals versus order", ylab="Stand. residuals",xlab="Order")
abline(0,0)
plot(fitted(lmw), lmw.stdres, col = "black", pch = 20,
     xlab = "Fitted", ylab = "Stand. Residuals", main = "Stand. Residuals versus Fitted")
abline(0,0)

shapiro.test(lmw.stdres)

durbinWatsonTest(lmw)



lm.mfp<-mfp(sales~fp(youtube, df = 4, select = 0.1)+fp(facebook, df = 4, select = 0.1)+fp(newspaper, df = 4, select = 0.1),data=marketing)


#mfp results
print(lm.mfp)

#diagnostic plots
par(mai=rep(0.5, 4))
layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE))

lm.mfp.stdres = rstandard(lm.mfp)

#qq plot
qqPlot(lm.mfp.stdres,main="Normal Q-Q Plot")
plot(lm.mfp.stdres, pch = 16, col = "black",main="Stand. Residuals versus order", ylab="Stand. residuals",xlab="Order")
abline(0,0)

#stand. residuals vs fitted values
plot(fitted(lm.mfp), lm.mfp.stdres, col = "black", pch = 20,
     xlab = "Fitted", ylab = "Stand. Residuals", main = "Stand. Residuals versus Fitted")
abline(0,0)

#tests
shapiro.test(lm.mfp.stdres)
durbinWatsonTest(lm.mfp)

#coefficiend of determination, R2
cor(marketing$sales, lm.mfp$fitted.values)^2


fpyoutube<-function(x){
        2.673*log((x/100))+ 2.283*((x/100)^1)
}
fpfacebook<-function(x){
        -2.954*(((x+0.2)/10)^0.5)+ 3.306*(((x+0.2)/10)^0.5*log(((x+0.2)/10)))
}
fpregression<-function(x1,x2){
        11.250 + 2.673*log((x1/100))+ 2.283*((x1/100)^1)-2.954*(((x2+0.2)/10)^0.5)+ 3.306*(((x2+0.2)/10)^0.5*log(((x2+0.2)/10)))
}

x1<-seq(0.2, 360, length= 50)
x2<-seq(0, 60, length= 50)

par(mai=rep(0.5, 4))
layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE))
plot(x1,fpyoutube(x1),type="l",main="fp for youtube")
plot(x2,fpfacebook(x2),type="l",main="fp for facebook")
z <- outer(x1, x2, fpregression)

persp(x1, x2, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue",main="regression surface",xlab="youtube",ylab="facebook")


par(mfrow=c(1, 1))
bc<-boxcox(lm.mfp, plotit = TRUE,lambda = seq(-1, 1, 1/10))
lambda <- bc$x[which.max(bc$y)]
lambda


lm.mfp.l<-mfp(((sales^lambda-1)/lambda)~fp(youtube, df = 4, select = 0.1)+fp(facebook, df = 4, select = 0.1)+fp(newspaper, df = 4, select = 0.1),data=marketing)
print(lm.mfp.l)
par(mai=rep(0.5, 4))
layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE))

lm.mfp.l.stdres = rstandard(lm.mfp.l) 
qqPlot(lm.mfp.l.stdres,main="Normal Q-Q Plot")
plot(lm.mfp.l.stdres, pch = 16, col = "black",main="Stand. Residuals versus order", ylab="Stand. residuals",xlab="Order")
abline(0,0)
plot(fitted(lm.mfp.l), lm.mfp.l.stdres, col = "black", pch = 20,
     xlab = "Fitted", ylab = "Stand. Residuals", main = "Stand. Residuals versus Fitted")
abline(0,0)

shapiro.test(lm.mfp.l.stdres)

durbinWatsonTest(lm.mfp.l)

cor(marketing$sales, lm.mfp.l$fitted.values)^2main


library(car)
library(ggplot2)
challenger <- read.delim("challenger.txt")
head(challenger)
par(mai=rep(0.75, 4.1))


par(mai=rep(0.75, 4.1))
layout(matrix(c(1,2,3,3), ncol = 2, byrow = TRUE))

with(challenger[challenger$nfails.field>0,], plot(temp,nfails.field,ylim=c(0,max(nfails.field)), main="o-ring fails in the field joints (n>0)", ylab="number of fails", xlab="Temperature", xlim=c(-1,30)))
plot(challenger$temp,challenger$nfails.field,ylim=c(0,max(challenger$nfails.field)), main="o-ring fails in the field joints (all flights)", ylab="number of fails", xlab="Temperature", xlim=c(-1,30))

plot(challenger$temp,challenger$fail.field, main="o-ring (field joints) fail or not - all flights including Challenger", ylab="Fail", xlab="Temperature", xlim=c(-1,30))
points(-0.6,1,pch=16)
text(-0.6,1,label="Challenger", pos=4)




fit1<-glm(fail.field~temp+pres.field, family="binomial",data=challenger)
summary(fit1)

x<-seq(-1,30, length=100)
y<-exp(-(fit1$coefficients[1]+fit1$coefficients[2]*x+fit1$coefficients[3]*mean(challenger$pres.field)))
y<-1/(1+y)
par(mai=c(1,1,1,1))
plot(challenger$temp,challenger$fail.field, main="o-ring (field joints) fail or not - all flights including Challenger", ylab="Fail", xlab="Temperature", xlim=c(-1,30))
points(-0.6,1,pch=16)
text(-0.6,1,label="Challenger", pos=4)
lines(x,y,col=2)


anova(fit1,test="Chisq")


fit0<-glm(fail.field~1, family="binomial",data=challenger)
fitfinal<-glm(fail.field~temp, family="binomial",data=challenger)

R2D1<-1-fit1$deviance/fit0$deviance
R2Dfinal<-1-fitfinal$deviance/fit0$deviance
R2D<-c(0,R2Dfinal,R2D1)

AICModels<-c(AIC(fit0),AIC(fitfinal),AIC(fit1))

gof<-cbind(R2D,AICModels)
rownames(gof)<-c("NULL","temp","temp+pres.field")

stepAIC(fit0,direction="both",scope=list(upper=fit3,lower=fit0))

summary(fitfinal)



predict(fitfinal, newdata=data.frame(temp=c(-0.6,20)),type="response")


predictCI<-function(object, newobs, level){
  
  prediction<-predict(object, newdata=newobs,se.fit=TRUE) #odds-ratio
  
  z <- qnorm((1-level)/2)
  low <- prediction$fit+z*prediction$se.fit
  up <- prediction$fit-z*prediction$se.fit
  
  fit <- 1/(1+exp(-prediction$fit))
  LowerLimit <- 1/(1+exp(-low))
  UpperLimit <- 1/(1+exp(-up))
  
  results <- cbind(newobs,fit,LowerLimit,UpperLimit)
  colnames(results)<- c("newobs","fit","Lower Limit","Upper Limit")
  return(results)
}

predictCI(fitfinal, newobs=data.frame(temp=c(-0.6,20)), 0.95)




#diagnostics
library("gridExtra")

fig1<-qplot(predict(fitfinal),residuals.glm(fitfinal))+
  geom_smooth()+
  labs(x = "Predicted values",
       y = "Deviance residuals)")
    
    
fig2<-ggplot(data = dt, aes(x = date, y = residuals.glm(fitfinal))) +
  geom_point() +
  labs(x = "Date",
       y = "Deviance residuals)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


grid.arrange(fig1, fig2, ncol = 2, nrow = 1)

#correct classification
fitfinal.probs <- predict(fitfinal,type = "response")
fitfinal.pred <- ifelse(fitfinal.probs > 0.5, 1, 0)
table(fitfinal.pred,challenger$fail.field)

######################################














######################################
##Poisson

fitn<-glm(nfails.field~temp+pres.field, family="poisson",data=challenger)
summary(fitn)




######################################
fitn0<-glm(nfails.field~1, family="poisson",data=challenger)
anova(fitn,test="Chisq")
fitnfinal<-glm(nfails.field~temp, family="poisson",data=challenger)

R2D1<-1-fitn$deviance/fit0$deviance
R2Dfinal<-1-fitnfinal$deviance/fit0$deviance
R2D<-c(0,R2Dfinal,R2D1)
AICModels<-c(AIC(fitn0),AIC(fitnfinal),AIC(fitn))
gof<-cbind(R2D,AICModels)
rownames(gof)<-c("NULL","temp","temp+pres.field")
gof

summary(fitnfinal)

predictCI(fitnfinal, newobs=data.frame(temp=c(-0.6,20)), 0.95)


dt<-cbind(challenger,residuals.glm(fitnfinal))

fig1<-qplot(predict(fitnfinal),residuals.glm(fitnfinal))+
  geom_smooth()+
  labs(x = "Predicted values",
       y = "Deviance residuals)")

fig2<-ggplot(data = dt, aes(x = date, y = residuals.glm(fitnfinal))) +
  geom_point() +
  labs(x = "Date",
       y = "Deviance residuals)")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(fig1, fig2, ncol = 2, nrow = 1)

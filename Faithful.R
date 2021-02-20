library(datasets)
OldFaithful<-faithful
eruptions <- OldFaithful$eruptions
waiting <-  OldFaithful$waiting
par(mfrow=c(1,2))
hist(eruptions, main="Eruptions")
hist(waiting, main="Waiting")

par(mfrow=c(1,2))
boxplot(eruptions, main="Eruptions")
boxplot(waiting, main="Waiting")


c(mean(eruptions),sd(eruptions))
summary(OldFaithful)



meanOF <- sapply(OldFaithful, mean)

sdOF <- sapply(OldFaithful, sd)

cvOF <- sdOF / abs(meanOF)
sapply(OldFaithful, IQR)
sapply(OldFaithful, quantile,probs=c(0.1,0.9))


library(e1071)
sapply(OldFaithful, skewness)
sapply(OldFaithful, kurtosis)


par(mfrow=c(1,1))
cor(waiting,eruptions)
plot(waiting,eruptions, 
     main = "faithful data: Eruptions of Old Faithful",
     ylab = "Eruption time (min)",
     xlab = "Waiting time to next eruption (min)")


groups <- ifelse(eruptions>3,1,0)





library(ggplot2)

ggplot(OldFaithful) +
  aes(x = waiting, y = eruptions,  colour = factor(groups)) +
  geom_point() +
  scale_colour_discrete(name = "eruptions>3 min", labels = c("No", "Yes"))

OldFaithful<-cbind(OldFaithful,groups)
by(OldFaithful, groups, summary)


par(mfrow=c(1,2))
boxplot(eruptions~groups,xlab ="group",ylab ="eruptions")

boxplot(waiting~groups,xlab ="group",ylab ="waiting")

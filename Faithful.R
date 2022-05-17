library(datasets)
library(e1071)
library(ggplot2)

OldFaithful<-faithful
eruptions <- OldFaithful$eruptions
waiting <-  OldFaithful$waiting

# mean
(MeanOldF <- c(mean(eruptions),mean(waiting)))

# median
(MeanOldF <- c(median(eruptions),median(waiting)))

# min
(MinOldF <- c(min(eruptions),min(waiting)))

# max
(MaxOldF <- c(max(eruptions),max(waiting)))

# quartiles & quantiles
# eruptions
(qEruptions <- quantile(eruptions,probs=c(0.1,0.25,0.75,0.9)))
# waiting
(qwaiting <- quantile(waiting,probs=c(0.1,0.25,0.75,0.9)))

# standard deviation
(sdOldF <- c(sd(eruptions),sd(waiting)))

# variance
(varOldF <-c(var(eruptions),var(waiting)))

# coefficient of variation
(cvOldF <- sdOldF/abs(MeanOldF))

# range
(RangeOkdF <- c(diff(range(eruptions)),diff(range(waiting))))

# IQR
(IQROkdF <- c(IQR(eruptions),IQR(waiting)))

# skewness
(skewnessOkdF <- c(skewness(eruptions),skewness(waiting)))

# kurtosis
(kurtosisOkdF <- c(kurtosis(eruptions,type = 1),kurtosis(waiting,type = 1)))

# summary of basic descriptive statistics 
summary(OldFaithful)

sapply(OldFaithful, IQR)
sapply(OldFaithful, quantile,probs=c(0.1,0.25,0.75,0.9))

par(mfrow=c(1,2))
hist(eruptions, main="Eruptions")
hist(waiting, main="Waiting")

par(mfrow=c(1,2))
boxplot(eruptions, main="Eruptions")
boxplot(waiting, main="Waiting")

par(mfrow=c(1,1))
cor(waiting,eruptions)
plot(waiting,eruptions, 
     main = "faithful data: Eruptions of Old Faithful",
     ylab = "Eruption time (min)",
     xlab = "Waiting time to next eruption (min)")

groups <- ifelse(eruptions>3,1,0)
table(groups)

ggplot(OldFaithful) +
  aes(x = waiting, y = eruptions,  colour = factor(groups)) +
  geom_point() +
  scale_colour_discrete(name = "eruptions>3 min", labels = c("No", "Yes"))

by(OldFaithful, groups, summary)

par(mfrow=c(1,2))
boxplot(eruptions~groups,xlab ="group",ylab ="eruptions")
boxplot(waiting~groups,xlab ="group",ylab ="waiting")


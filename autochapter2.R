library(ISLR)

# auto 1
FreqTable <- function(data,cum){
  Freq <-table(data)
  RelFreq <- prop.table(Freq)
  RelFreq_p <-RelFreq*100
  if(cum==TRUE){
    Cum_RelFreq <- cumsum(RelFreq)
    Cum_RelFreq_p <- Cum_RelFreq*100
    tab <- cbind(Freq,RelFreq,RelFreq_p,Cum_RelFreq,Cum_RelFreq_p)
  }else{
    tab <- cbind(Freq,RelFreq,RelFreq_p)
  }
  return(tab)
}

FreqTable(Wage$race,cum=FALSE)
FreqTable(Wage$education,cum=FALSE)
FreqTable(Wage$jobclass,cum=FALSE)
FreqTable(Wage$health,cum=TRUE)
FreqTable(Wage$health_ins,cum=FALSE)

# auto 2
library(gmodels)
CrTab <- CrossTable(Wage$education,Wage$jobclass,prop.chisq=FALSE)

# auto 4
library(datasets)
library(e1071)
library(ggplot2)
library(beanplot)

OldFaithful<-faithful
eruptions <- OldFaithful$eruptions
waiting <-  OldFaithful$waiting
groups <- ifelse(eruptions>3,1,0)


DesFUNGroup = function(x,probs,group) {
  n= tapply(x,group,length)
  m = tapply(x,group,mean)
  s = tapply(x,group,sd)
  cv=s/abs(m)
  results <- rbind(n,m,s,cv)
  for(i in 1:length(probs)){
    results <- rbind(results,tapply(x,groups,quantile,probs=probs[i]))
  }
  results <- rbind(results,tapply(x,group,skewness))
  results <- rbind(results, tapply(x,group,kurtosis))
  rownames(results) <- c("n","mean","sd","cv",probs,"sk","ku")
  return(results)
}


DesTABLEeruption<-DesFUNGroup(eruptions,probs,groups)
colnames(DesTABLEeruption) <- c("eruptions<3 min","eruptions >3 min")
DesTABLEeruption

DesTABLEwaiting<-DesFUNGroup(waiting,probs,groups)
colnames(DesTABLEwaiting) <- c("eruptions<3 min","eruptions >3 min")
DesTABLEwaiting

par(mfrow=c(1,2))
beanplot(eruptions~groups,xlab ="eruptions>3 min",ylab ="eruptions", col="bisque",method="jitter")
beanplot(waiting~groups,xlab ="eruptions>3 min",ylab ="waiting", col="bisque",method="jitter")


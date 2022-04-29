rm(list=ls())
library(survival)
km.by.sex <- survfit(Surv(time, status)~sex, data = lung, conf.type = "log-log")
ggsurvplot(km.by.sex, data = lung,  conf.int = TRUE)


library("coin")
All.test<-function(survival_object,data){
  all.types = c("logrank", "Gehan-Breslow", "Tarone-Ware",
                "Peto-Peto", "Prentice", "Prentice-Marek",
                "Andersen-Borgan-Gill-Keiding",
                "Fleming-Harrington", "Gaugler-Kim-Liao", "Self")
  
  all.tests<-matrix(NaN,nrow=length(all.types),ncol=2)
  for( i in 1:length(all.types)){
    running.test<-logrank_test(survival_object, data = data,type = all.types[i],rho=0.5)  
    all.tests[i,]<-c(statistic(running.test),pvalue(running.test))
  }
  rownames(all.tests)<-all.types
  colnames(all.tests)<-c("value","p-value")
  return(all.tests)
}
All.test(Surv(time,  status) ~ factor(sex),lung)



cmx <- coxph(Surv(time, status) ~., data=veteran)
zph <- cox.zph(cmx , transform="identity")
zph


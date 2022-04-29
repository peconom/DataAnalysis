library(survival)
str(veteran)
?veteran

library(survminer)
km.as.one <- survfit(Surv(time, status)~ 1, data = veteran)
summary(km.as.one)
ggsurvplot(km.as.one, data = veteran)


library(gridExtra)
km.by.trt <- survfit(Surv(time, status)~trt, data = veteran, conf.type = "log-log")
p1 <- ggsurvplot(km.by.trt, data = veteran, conf.int = TRUE)

km.by.celltype <- survfit(Surv(time, status)~celltype, data = veteran, conf.type = "log-log")
p2 <- ggsurvplot(km.by.celltype, data = veteran, conf.int = TRUE)
grid.arrange(p1$plot, p2$plot,ncol = 2, nrow = 1)

survdiff(Surv(time, status)~trt, data = veteran, rho=0)
survdiff(Surv(time, status)~celltype, data = veteran, rho=0)


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


All.test(Surv(time,  status) ~ factor(trt),veteran)
All.test(Surv(time,  status) ~ factor(celltype),veteran)

cmx <- coxph(Surv(time, status) ~., data=veteran)
summary(cmx)

ggforest(cmx , data = veteran)

library(MASS)
fitb = stepAIC(cmx, direction="backward", k=2)


ggforest(fitb , data = veteran)

(zph <- cox.zph(fitb , transform="identity"))
ggcoxzph(zph)


llplot <- function(x, 
                   main = NULL, 
                   ylab = "-Log(-Log(Survival))", xlab = "Log(t)") { 
  
  strata <- cbind(1:sum(x$strata), rep(1:length(x$strata), x$strata)) 
  ind <- split(strata[, 1], strata[, 2]) 
  
  finite.y <- is.finite(-log(-log(x$surv))) 
  ylim <- range(-log(-log(x$surv))[finite.y]) 
  
  xlim <- log(c(min(x$time), max(x$time))) 
  
  plot(xlim, ylim, type = "n", main = main, 
       ylab = ylab, xlab = xlab) 
  
  for (i in seq(along = ind)) 
  { 
    lines(log(x$time[ind[[i]]]), 
          -log(-log(x$surv[ind[[i]]])), 
          lty = i, col=i, type = "o") 
  } 
  
  legend(0.8*xlim[2], 0.95*ylim[2], legend=(names(x$strata)),
         col=seq(along = ind), lty=seq(along = ind), cex=0.8)
}

fit<-survfit(Surv(time, status) ~ celltype, data=veteran)
llplot(fit)

veteran$cat.karno <- as.factor(ifelse(veteran$karno < 40, 'A',
                                      ifelse(veteran$karno< 70, 'B', 
                                             ifelse(veteran$karno < 100, 'C'))))

veteran$cat.cell <- as.factor(ifelse(veteran$celltype == "squamous", 'A',
                                     ifelse(veteran$celltype == "large", 'A', 
                                            ifelse(veteran$celltype == "smallcell", 'B', 
                                                   ifelse(veteran$celltype == "adeno", 'C',NA)))))

fit.cat<-coxph(Surv(time, status) ~ cat.karno+cat.cell, data=veteran)
summary(fit.cat)
ggforest(fit.cat , data = veteran)    

zph <- cox.zph(fit.cat , transform="identity")
zph

fit.cat.karno<-survfit(Surv(time, status) ~cat.karno , data=veteran)
llplot(fit.cat.karno)

fit.cat.cell<-survfit(Surv(time, status) ~cat.cell , data=veteran)
llplot(fit.cat.cell)
          

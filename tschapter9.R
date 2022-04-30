library(datasets)
plot(sunspot.month,main="sunspots by month")
plot(EuStockMarkets[,1],main="Daily Closing Prices",ylab="DAX")
plot(co2, main="Annual Carbon Dioxide Uptake in Grass Plants")
plot(LakeHuron, main="Annual measurements of Lake Huron level",ylab="feet")


plot(AirPassengers, main="Monthly totals of international airline passengers")
plot(log(AirPassengers), main="Monthly totals of international airline passengers (log scale)")

library(Kendall)
#LakeHuron
MannKendall(LakeHuron)
D_LakeHuron<-diff(LakeHuron,diff=1)
plot(D_LakeHuron, main="Annual Differences of Lake Huron level",ylab="feet")
MannKendall(D_LakeHuron)


#log(AirPassengers)

library(forecast)
training <- subset(AirPassengers, end=length(AirPassengers)-12)
test <- subset(AirPassengers, start=length(AirPassengers)-11)

MannKendall(log(training))

#fit linear
t<-seq(1:length(log(training)))
df=data.frame(logAir=log(training),
              t=t)

trend1<-lm(logAir~t,df)
trend2<-lm(logAir~poly(t,2,raw=TRUE),df)
trend3<-nls(logAir~I(a* b^t),df,start=list(a=1,b=.2))
trend4<-nls(logAir ~ SSlogis(t, asymp, xmid, scal),df)
            
            

plot(df$t,df$logAir,type="l",xlab="t",ylab="log(AirPass)")
lines(df$t, predict(trend1, df$logAir), col="blue", type="l", lty=2)
lines(df$t, predict(trend2, df$logAir), col="red", type="l", lty=2)
lines(df$t, predict(trend3, df$logAir), col="black", type="l", lty=2)
lines(df$t, predict(trend4, df$logAir), col="green", type="l", lty=2)

legend(1, 6, legend=c("linear", "Quandratic","Exp Growth","Logistic "),
       col=c("blue", "red","black","green"), lty=1:2, cex=0.8)




AccuracyMeasures<-function(obs,fit,object){
  if(any(obs==0)){
    MAPE=NaN
  }else{
    MAPE=sum(abs((obs-fit)/obs)/length(fit)*100)
  }
  
  MAD=sum(abs(obs-fit))/length(fit)
  MSD=sum((obs-fit)^2)/length(fit)
  if(missing(object)){
    return(list(MAPE=MAPE,MAD=MAD,MSD=MSD))
  }else{
    return(list(MAPE=MAPE,MAD=MAD,MSD=MSD,AIC=AIC(object),BIC=BIC(object)))
    }
}
  
M1<-AccuracyMeasures(df$logAir,trend1$fitted.values,trend1)
M2<-AccuracyMeasures(df$logAir,trend2$fitted.values,trend2)
M3<-AccuracyMeasures(df$logAir,predict(trend3),trend3)
M4<-AccuracyMeasures(df$logAir,predict(trend4),trend4)
rbind(M1, M2, M3, M4)


par(mfrow=c(1,2))
newt<-data.frame(t=seq(132+1,132+12*1))
plot(data.frame(newt, as.vector(predict(trend2, new=newt))), col="red", type="l", lty=2,
     xaxt="n",xlab="1960/month",ylab="log(AirPass)")
lines(data.frame(newt, predict(trend4, new=newt)), col="green", type="l", lty=2)
axis(side=1, at=newt$t, labels=c("Jan","Feb","Mar",
         "Apr","May","Jun",
         "Jul","Aug","Sep",
         "Oct","Nov","Dec"))

legend(134, 6.17, legend=c("Quandratic","Logistic"),
       col=c("red","green"), lty=1:2, cex=0.8)


newt<-data.frame(t=seq(132+1,132+12*20))
plot(data.frame(newt, as.vector(predict(trend2, new=newt))), col="red", type="l", lty=2,
     ylim=c(6,7),xaxt="n",xlab="year",ylab="log(AirPass)")
lines(data.frame(newt, predict(trend4, new=newt)), col="green", type="l", lty=2)
axis(side=1, at=seq(132+1,132+1+12*20,12), labels=seq(1960,1980))



par(mfrow=c(1,1))

detrendM4<-df$logAir-predict(trend2)
plot(detrendM4,type="l")

MannKendall(detrendM4)

mx=40
acf(detrendM4,lag.max=mx, xaxt="n", xlab="Lag (months)")
axis(1, at=0:mx/12, labels=0:mx)


acf(sunspots,lag.max=400, xaxt="n", xlab="Lag (years)")
axis(1, at=0:400/12, labels=0:400)



dfM4=data.frame(detrendM4=as.vector(detrendM4),
              month= factor(rep(seq(1:12),length(training)/12))
)

library(ggplot2)
library(reshape2)
library(dplyr)
dfM4.long<-melt(dfM4,id.vars="month")
ggplot(dfM4.long,aes(month,value))+geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=20, size=6, color="red", fill="red")+
  ylab("detrend log(AirPass)")


summary_monthly_detrend_logAirPass <- dfM4 %>% 
  group_by(month) %>% 
  summarize(mean = mean(detrendM4, na.rm = TRUE), 
            median = median(detrendM4, na.rm = TRUE))
summary_monthly_detrend_logAirPass


Seasonalindices_mean<-summary_monthly_detrend_logAirPass$mean
Seasonalindices_median<-summary_monthly_detrend_logAirPass$median


M4S1<-AccuracyMeasures(detrendM4,rep(Seasonalindices_mean,length(training)/12))
M4S2<-AccuracyMeasures(detrendM4,rep(Seasonalindices_median,length(training)/12))
cbind(M4S1,M4S2)



      
detrendM4_deseasonal<-detrendM4-rep(Seasonalindices_median,length(training)/12)
plot(detrendM4_deseasonal,type="l")




     

     
     
#Stationarity
library(tseries)
adf.test(detrendM4_deseasonal)


#ARIMA ORDER 
par(mfrow=c(1,2))
mx=40
acf(detrendM4_deseasonal,lag.max=mx, xaxt="n", xlab="Lag (months)")
axis(1, at=0:mx/12, labels=0:mx)

pacf(detrendM4_deseasonal,lag.max=mx, xaxt="n", xlab="Lag (months)")
axis(1, at=1:mx/12, labels=1:mx)

#fit ARIMA
fitARIMA <- arima(detrendM4_deseasonal, order=c(1,0,0),seasonal = list(order = c(1, 0, 0), period = 12),method="ML",include.mean = FALSE)
fitARIMA

par(mfrow=c(1,2))
mx=40
aaa<-acf(fitARIMA$residuals,lag.max=mx, xaxt="n", xlab="Lag (months)")
axis(1, at=0:mx/12, labels=0:mx)

pacf(fitARIMA$residuals,lag.max=mx, xaxt="n", xlab="Lag (months)")
axis(1, at=1:mx/12, labels=1:mx)




library(FitAR)
library(car)
boxresult<-LjungBoxTest(fitARIMA$residuals,k=2,StartLag=1, lag.max=30)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag",ylim=c(0,1))
abline(h = 0.05)
axis(1, at=1:30)

qqPlot(fitARIMA$residuals,main="Normal Q-Q Plot")
shapiro.test(fitARIMA$residuals)

library(lmtest)
coeftest(fitARIMA)


#fit auto.arima
library(forecast)
fitAutoARIMA<-auto.arima(detrendM4_deseasonal, trace=TRUE)
fitAutoARIMA

boxresult<-LjungBoxTest(fitAutoARIMA$residuals,k=2,StartLag=1, lag.max=30)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag",ylim=c(0,1))
abline(h = 0.05)
axis(1, at=1:30)

qqPlot(fitAutoARIMA$residuals,main="Normal Q-Q Plot")
shapiro.test(fitAutoARIMA$residuals)
coeftest(fitAutoARIMA)


#forecast
#ARIMA
nyears<-1
forecastARIMA<-forecast::forecast(fitAutoARIMA,h=12)

autoplot(forecastARIMA)

#plus seasonality + trend
forecasttrend<-predict(trend4, newdata=data.frame(t=length(dfM4$month)+seq(1,nyears*12)))[1:(nyears*12)]
forecastseasonoal<-rep(Seasonalindices_median,nyears)

Pointforecast<-as.vector(forecastARIMA$mean)+forecastseasonoal+forecasttrend
Limitlower<-as.matrix(forecastARIMA$lower)+forecastseasonoal+forecasttrend
Limitupper<-as.matrix(forecastARIMA$upper)+forecastseasonoal+forecasttrend

#forecastdata<-data.frame(Point=Pointforecast,
#                         L=Limitlower,
#                         U=Limitupper
#)


library(dplyr)
library(lubridate)

#exp (original scale)
forecastoriginalscale<-data.frame(Point=exp(Pointforecast),
                                  L=exp(Limitlower),
                                  U=exp(Limitupper),
                                  month =list(seq(floor_date(ymd("1960-01-01"),unit = "month"), 
                                                          floor_date(ymd("1960-12-31"), unit = "month"), by = "month"))
                                  
)




Forecasts<-ts(forecastoriginalscale$Point, start = c(1960, 1),freq=12)

#plot(c(actuals=as.vector(AirPassengers), forecast_mean=forecastoriginalscale$Point),type="l")

Allts<- ts(c(training, Forecasts),               # Combined time series object
   start = start(AirPassengers),
   frequency = frequency(AirPassengers))


All<- data.frame(obs=Allts,  
                 month=time(Allts),
                 L80=c(rep(NaN,length(training)),forecastoriginalscale$L.80),
                 L95=c(rep(NaN,length(training)),forecastoriginalscale$L.95),
                 U80=c(rep(NaN,length(training)),forecastoriginalscale$U.80),
                 U95=c(rep(NaN,length(training)),forecastoriginalscale$U.95),
                 group=c(rep(0,length(training)),rep(1,length(Forecasts)))
                 )

df_test <- data.frame(test=test,
                      month=time(AirPassengers)[133:144]
)

p1<-  ggplot(All) +
    geom_line(aes(month, obs, colour = factor(group))) +
    theme(legend.position = "none") +
    geom_ribbon(aes(x=month, ymin=L80, ymax=U80), alpha=0.2,fill = "steelblue2") +
    geom_ribbon(aes(x=month, ymin=L95, ymax=U95), alpha=0.2,fill = "coral")+  
    geom_point(data=df_test,aes(month,test))


p2<-  ggplot(All) +
  geom_line(aes(month, obs, colour = factor(group))) +
  theme(legend.position = "none") +
  geom_ribbon(aes(x=month, ymin=L80, ymax=U80), alpha=0.2,fill = "steelblue2") +
  geom_ribbon(aes(x=month, ymin=L95, ymax=U95), alpha=0.2,fill = "coral")+
  coord_cartesian(xlim=c(1960,1961),ylim=c(350,660))+  
  geom_point(data=df_test,aes(month,test))+
  scale_x_continuous(breaks=seq(1960, 1961-1/12, 1/12),labels=c("Jan","Feb","Mar",
                                                           "Apr","May","Jun",
                                                           "Jul","Aug","Sep",
                                                           "Oct","Nov","Dec"))


p1 + annotation_custom(ggplotGrob(p2), xmin =1949, xmax = 1956, 
                       ymin =400, ymax = 700)




hw.Forecasts <- hw(training,h = 12,level = c(80,95), model = "AAA")              

Allts_hw<- ts(c(training, hw.Forecasts$mean),               # Combined time series object
           start = start(AirPassengers),
           frequency = frequency(AirPassengers))

All_hw<- data.frame(obs=Allts_hw,  
                 month=time(Allts_hw),
                 L80=c(rep(NaN,length(training)),hw.Forecasts$lower[,1]),
                 L95=c(rep(NaN,length(training)),hw.Forecasts$lower[,2]),
                 U80=c(rep(NaN,length(training)),hw.Forecasts$upper[,1]),
                 U95=c(rep(NaN,length(training)),hw.Forecasts$upper[,2]),
                 group=c(rep(0,length(training)),rep(1,length(Forecasts)))
)


p1<-  ggplot(All_hw) +
  geom_line(aes(month, obs, colour = factor(group))) +
  theme(legend.position = "none") +
  geom_ribbon(aes(x=month, ymin=L80, ymax=U80), alpha=0.2,fill = "steelblue2") +
  geom_ribbon(aes(x=month, ymin=L95, ymax=U95), alpha=0.2,fill = "coral")+  
  geom_point(data=df_test,aes(month,test))


p2<-  ggplot(All_hw) +
  geom_line(aes(month, obs, colour = factor(group))) +
  theme(legend.position = "none") +
  geom_ribbon(aes(x=month, ymin=L80, ymax=U80), alpha=0.2,fill = "steelblue2") +
  geom_ribbon(aes(x=month, ymin=L95, ymax=U95), alpha=0.2,fill = "coral")+
  coord_cartesian(xlim=c(1960,1961),ylim=c(300,650))+  
  geom_point(data=df_test,aes(month,test))+
  scale_x_continuous(breaks=seq(1960, 1961-1/12, 1/12),labels=c("Jan","Feb","Mar",
                                                                "Apr","May","Jun",
                                                                "Jul","Aug","Sep",
                                                                "Oct","Nov","Dec"))

p1 + annotation_custom(ggplotGrob(p2), xmin =1949, xmax = 1956, 
                       ymin =380, ymax = 650)


FARIMA<-AccuracyMeasures(test,Forecasts)
FHW<-AccuracyMeasures(test,hw.Forecasts$mean)
rbind(FARIMA,FHW)


library(ISLR)


Freq <-table(Wage$maritl)
RelFreq <- prop.table(Freq)
RelFreq_p <-RelFreq*100
Cum_RelFreq<-cumsum(RelFreq)
Cum_RelFreq_p<-Cum_RelFreq*100

cbind(Freq,RelFreq,RelFreq_p)


barplot(Freq, main="Frequencies")
piepercent<- round(100*Freq/sum(Freq), 1)
categ <-  names(piepercent)
pie(Freq,main="Pie Chart of \n  marital status",col=rainbow(length(Freq)),labels = paste0(round(RelFreq_p,2), "%"))
legend("topleft", legend=categ,fill=rainbow(length(Freq)))


Freq <-table(Wage$health_ins)
RelFreq <- prop.table(Freq)
RelFreq_p <-RelFreq*100
CumRelFreq <-cumsum(RelFreq)
CumRelFreq_p <-CumRelFreq*100

cbind(Freq,RelFreq,RelFreq_p,CumRelFreq,CumRelFreq_p)



con1 <- table(Wage$jobclass,Wage$health_ins)

con1
addmargins(con1)

# overall
prop.table(con1)
# by row
prop.table(con1, margin = 1)
# by column
prop.table(con1, margin = 2)


mosaicplot(con1)



barplot(con1,
        main = "Frequencies",
        xlab = "health insurance",
        col = c("red","blue")
)
legend("topright",
       c("Industrial","Information"),
       fill = c("red","blue")
)



mean(as.numeric(Wage$health))


library(epiDisplay)
tab1(Wage$education, cum.percent = TRUE)

library(summarytools)
summarytools::freq(Wage$education)

library(gmodels)
CrTab <- CrossTable(Wage$jobclass,Wage$health_ins,prop.chisq=FALSE)

CrTab <- CrossTable(Wage$education,Wage$jobclass,prop.chisq=FALSE)








Time1 <-c(45,42, 36, 39, 51, 44)
Time2 <-c(50, 42,41, 35,55, 49)
Time3<- c(55, 45, 43, 40, 59, 56)
Time12 <-Time1-Time2
Time13 <-Time1-Time3
Time23 <-Time2-Time3
var(Time12)
var(Time13)
var(Time23)

dataWeight <- data.frame(Time1, Time2, Time3, id=factor(1:length(Time1)))
dataWeightMelted <-reshape2::melt(dataWeight,id.vars="id",variable.name="Time",
value.name = "Weight")
dataWeightMelted

library(ez)
ezANOVA(data = dataWeightMelted, dv = .(Weight),wid = .(id),within = .(Time),detailed = T,type = 3)


an1 <- aov(formula = Weight~Time + Error(id/Time),data = dataWeightMelted)
summary(an1)

pairwise.t.test(x = dataWeightMelted$Weight,g = dataWeightMelted$Time,paired = T,p.adjust.method = "bonf")

initData <- read.csv(file = "data_Sprint_Repeated_v1a.csv",stringsAsFactors = T)
summary(initData)


d3 <- reshape::melt(initData,id.vars="ids",variable_name="Period")
names(d3)[3] <- "SprintTime"

ggboxplot(data = d3,x = "Period", y = "SprintTime")
ggerrorplot(data = d3, x = "Period", y = "SprintTime",desc_stat = "mean_se")


by(data = d3$SprintTime,INDICES = d3$Period,FUN = shapiro.test)

library(ez)
an2 <- ezANOVA(data = d3,dv = .(SprintTime),wid = .(ids),within = .(Period),detailed = T,type = 3)
an2

pairwise.t.test(x = d3$SprintTime,g = d3$Period,paired = T,p.adjust.method = "bonf")

d3 <- read.xlsx(xlsxFile = "DESI-year-country.xlsx")
d3$country <- as.factor(d3$country)
d3$year <- as.factor(d3$year)

summary(d3)

by(data = d3$DESI,INDICES = d3$year,FUN = shapiro.test)


library(ez)
an2 <- ezANOVA(data = d3,dv = .(DESI),wid = .(country),within = .(year),detailed = T,type = 3)
an2

pairwise.t.test(x = d3$DESI,g = d3$year,paired = T,p.adjust.method = "bonf")


Month0 <-c(164,163, 166, 206, 167, 220, 162,172,183, 177)
Month1 <-c(162, 163,165, 200,166, 212,163,171,173,165)
Month2<- c(174, 162, 171, 184, 166, 207,161,148,165,162)

dataDiavitis <- data.frame(Month0 , Month1 , Month2, id=factor(1:length(Month0)))
dataDiavitisMelted <-reshape2::melt(dataDiavitis,id.vars="id",variable.name="Month",
                                  value.name = "Diavitis")
dataDiavitisMelted 

by(data = dataDiavitisMelted$Diavitis,INDICES = dataDiavitisMelted$Month,FUN = summary)

ezANOVA(data = dataDiavitisMelted, dv = .(Diavitis),wid = .(id),within = .(Month),detailed = T,type = 3)

pairwise.t.test(x = dataDiavitisMelted$Diavitis,g = dataDiavitisMelted$Month,paired = T,p.adjust.method = "bonf")

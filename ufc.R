rm(list=ls())
library(e1071)
library(FAwR)


DesFUN = function(x,probs) {
  results<- c(n = length(which(!is.na(x))),m = mean(x,na.rm = TRUE), s = sd(x,na.rm = TRUE),
               quantile(x,probs=probs,na.rm=TRUE),skewness(x,na.rm=TRUE),kurtosis(x,na.rm=TRUE)) 
  return(results)
}


probs=c(0,0.1,0.25,0.5,0.75,0.9,1)

#load data
data(ufc)
dbh<- ufc$dbh.cm
height<- ufc$height.m

DesTABLE<-matrix(NA,nrow=3+length(probs)+2,ncol=2)
DesTABLE[,1]<-DesFUN(dbh,probs)
DesTABLE[,2]<-DesFUN(height,probs)
rownames(DesTABLE) <- c("n","m","s","0%","10%","25%","50%","75%","90%","100%","sk","ku")
colnames(DesTABLE) <- c("dbh","height")

DesTABLE

tab1 <- rbind(table(species),100*table(species)/length(species))
rownames(tab1) <- c("Frequency","Percent")
tab1

par(mfrow=c(1,2))
hist(dbh, main="Diameter at breast height")
hist(height, main="height")



par(mfrow=c(1,2))
boxplot(dbh~ufc$species,xlab ="species",ylab ="Diameter at breast height")

boxplot(height~ufc$species,xlab ="species",ylab ="height")

library(beanplot)
beanplot(dbh~ufc$species,xlab ="species",ylab ="Diameter at breast height", col="bisque",method="jitter")
beanplot(height~ufc$species,xlab ="species",ylab ="height", col="bisque",method="jitter")

library(ggplot2)
ggplot(ufc) +
  aes(x = dbh, y = height,  colour = species) +
  geom_point() +
  scale_colour_discrete(name = "species", labels = c("DF", "GF", "WC","WL"))

ggplot(ufc) +
  aes(x = dbh, y = height) +
  geom_point() +  facet_grid(. ~ species)+ stat_smooth(method = "lm") +
  background_grid(major = 'y', minor = "none") 

treesperplot<-table(factor(ufc$plot,levels = 1:144))
locations <- data.frame(plot = 1:144,
                         north.n = rep(c(12:1),12),
                         east.n = rep(c(1:12), rep(12,12)))

par(mfrow=c(1,1))
plot(locations$east.n, locations$north.n,type = "n", axes = F,
      xlim = c(0,max(locations$east.n)),
      ylim = c(0,max(locations$north.n)),
      xlab = "West-East", ylab = "South-North",
      main = "area of each plot: 22482.2 m2")
 
axis(1,at = seq(1,12,by=1)); axis(2,at = seq(1,12,by=1), labels = paste(seq(12,1,by=-1)))
 
text(formatC(treesperplot,format = "f", digits = 0),
      x = locations$east.n,
      y = locations$north.n, 
      cex=1.5,
      col = gray(1 - (0.2+0.8*(treesperplot)/max(treesperplot))))

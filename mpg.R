rm(list=ls())
library(ggplot2)
data <- ggplot2::mpg
str(mpg)
hwydata<-data$hwy
#summary of the data in order to check the minimum and maximum value
summary(hwydata)

#second step histogram
hist(hwydata,xlab = "hwy", main = "Histogram of Çighway miles per gallon")
# third step box-plot
boxplot(hwydata,ylab = "hwy",main = "Boxplot of highway miles per gallon")

mtext(paste("Outliers: ", paste(boxplot.stats(hwydata)$out, collapse = ", ")))
out_ind1 <- which(hwydata %in% boxplot.stats(hwydata)$out)
out_ind1

possible1<-data$hwy[213]
possible2<-data$hwy[222]
possible3<-data$hwy[223]
possible1
possible2
possible3

#percentiles
outlier_ind2 <- which(hwydata < quantile(hwydata, 0.025)| hwydata > quantile(hwydata, 0.975))
outlier_ind2

outlier_ind3 <- which(hwydata < (mean(hwydata)-3*sd(hwydata)) | hwydata > (mean(hwydata)+3*sd(hwydata)))
outlier_ind3
#Hampel filter
outlier_ind4 <- which(hwydata < (mean(hwydata)-3*mad(hwydata,constant=1)) | hwydata > (mean(hwydata)+3*mad(hwydata,constant=1)))
outlier_ind4

library(outliers)
scoresz<-scores(hwydata,"z")
outlier_ind5 <- which(abs(scoresz)>1.96)
outlier_ind5
length(outlier_ind5)/length(scoresz)

outlier_ind6 <- which(abs(scoresz)>2.58)
outlier_ind6
length(outlier_ind6)/length(scoresz)

outlier_ind7 <- which(abs(scoresz)>3.29)
outlier_ind7
length(outlier_ind7)/length(scoresz)


#Rosner test
library(EnvStats)
rosnertest <- rosnerTest(hwydata,k = 3)
rosnertest




library(datasets)
str(PlantGrowth)

library(datasets)
str(ToothGrowth)

bartlett.test(weight~group, data = PlantGrowth)
library("lawstat") 
levene.test(PlantGrowth$weight, PlantGrowth$group, location="mean")



df <-data.frame(group = rep(c('1','2', '3'), each=10),
                 score = c(64, 66, 68, 75, 78, 94, 98, 79, 71, 80,
                           91, 92, 93, 85, 87, 84, 82, 88, 95, 86,
                         79, 78, 88, 94, 92, 85, 83, 85, 82,88 ))

bartlett.test(score~group, data = df)

levene.test(df$score, df$group, location="mean")

library(datasets)
oneway.test(weight~group,var.equal=TRUE, data = PlantGrowth)

modeloneway <- aov(formula =  weight~group,data =PlantGrowth)
summary(modeloneway)


df <-data.frame(group = rep(c('1','2', '3'), each=10),
                 score = c(64, 66, 68, 75, 78, 94, 98, 79, 71, 80,
                           91, 92, 93, 85, 87, 84, 82, 88, 95, 86,
                         79, 78, 88, 94, 92, 85, 83, 85, 82,88 ))
oneway.test(score~group,var.equal=FALSE, data = df)


library(datasets)
library(ggpubr)
ggboxplot(PlantGrowth, x = "group", y = "weight", 
          color = "group",
          ylab = "Weight", xlab = "Group")


library(DescTools)
PostHocTest(modeloneway, method = "lsd")
pairwise.t.test(PlantGrowth$weight, PlantGrowth$group, p.adj = "none")
pairwise.t.test(PlantGrowth$weight, PlantGrowth$group, p.adj = "bonf")
pairwise.t.test(PlantGrowth$weight, PlantGrowth$group, p.adj = "holm")
TukeyHSD(modeloneway)


ggline(PlantGrowth, x = "group", y = "weight", 
       add = c("mean_ci", "jitter"), 
       ylab = "Weight", xlab = "Group")



df <-data.frame(group = rep(c('A','B', 'C'), each=10),
                score = c(64, 66, 78, 75, 78, 94, 98, 79, 71, 80,
                          91, 92, 93, 85, 87, 84, 82, 88, 95, 86,
                          79, 78, 88, 94, 92, 85, 83, 85, 82,88 ))
library(rstatix)                          
games_howell_test(df, score~group, conf.level = 0.95)
library(PMCMRplus)
df$group <- as.factor(df$group)
tamhaneT2Test(score~group, df)
dunnettT3Test(score~group, df)


kruskal.test(PlantGrowth$weight, PlantGrowth$group) 


d1 <- read.csv("datasmoking.csv",stringsAsFactors = T)
summary(d1)
by(data = d1$Sprint,INDICES = d1$Smoking,FUN = summary)

by(data = d1$Sprint,INDICES = d1$Smoking,FUN = shapiro.test)

bartlett.test(x = d1$Sprint,g=d1$Smoking)

res.aov <- aov(formula = Sprint~Smoking,data = d1)
summary(res.aov)


pairwise.t.test(x = d1$Sprint,g = d1$Smoking,p.adjust.method = "holm" )
pairwise.t.test(x = d1$Sprint,g = d1$Smoking,p.adjust.method = "bonf" )
TukeyHSD(res.aov)

d1 <- read.csv("data_tetris_v1a.csv",stringsAsFactors = T)
summary(d1)

by(data = d1$NumberOfIntrusiveMemories,INDICES = d1$Condition,FUN = summary)
boxplot(formula=NumberOfIntrusiveMemories~Condition, data=d1)

by(data = d1$NumberOfIntrusiveMemories,INDICES = d1$Condition,FUN = shapiro.test)

kruskal.test(x = d1$NumberOfIntrusiveMemories,g = d1$Condition)

pairwise.wilcox.test(x = d1$NumberOfIntrusiveMemories,g = d1$Condition,p.adjust.method = "holm")
pairwise.wilcox.test(x = d1$NumberOfIntrusiveMemories,g = d1$Condition,p.adjust.method = "bonf")


ToothGrowth$dose <- factor(ToothGrowth$dose, 
                           levels = c(0.5, 1, 2),
                           labels = c("D0.5", "D1", "D2")
                           )

library(dplyr)
# Normality test for each group
ToothGrowth %>%
  group_by(dose, supp) %>% 
  summarise(n=n(),
            statistic = shapiro.test(len)$statistic,
            p.value = shapiro.test(len)$p.value)
#Homogeneity of Variance
library(car)
leveneTest(len ~ dose*supp  , data = ToothGrowth)

aov.res <- aov(len ~ dose + supp +  dose:supp, data = ToothGrowth)
summary(aov.res)

library(lsmeans)
Tuk<-lsmeans(aov.res, list(pairwise~dose|supp,pairwise~supp|dose,pairwise~dose+supp),  adjust="tukey")
Tuk

plot(Tuk[[1]])
plot(Tuk[[2]])  
plot(Tuk[[3]])  
plot(Tuk[[4]])  
plot(Tuk[[5]])  
plot(Tuk[[6]])  


library(datarium)
library(dplyr)
# Normality test for each group
jobsatisfaction %>%
  group_by(education_level, gender) %>% 
  summarise(n=n(),
            statistic = shapiro.test(score)$statistic,
            p.value = shapiro.test(score)$p.value)
#Homogeneity of Variance
library(car)
leveneTest(score ~ education_level*gender  , data = jobsatisfaction)


model = lm(score ~ education_level + gender +  education_level:gender, data = jobsatisfaction)
aov.III <- Anova(model, type="III")
aov.III

library(palmerpenguins)
str(penguins)

by(data=penguins$flipper_length_mm,INDICES = penguins$species,FUN = summary)

by(data = penguins$flipper_length_mm,INDICES = penguins$species,FUN = shapiro.test)


library("lawstat") 
leveneTest(penguins$flipper_length_mm, penguins$species, location="mean")

res.aov <- aov(formula = flipper_length_mm~species,data = penguins)
summary(res.aov)

pairwise.t.test(x = penguins$flipper_length_mm,g = penguins$species,p.adjust.method = "holm" )
pairwise.t.test(x = penguins$flipper_length_mm,g = penguins$species,p.adjust.method = "bonf" )
TukeyHSD(res.aov)




library(lsmeans)
Tuk<-lsmeans(model.edu.inter, list(pairwise~education_level:gender),  adjust="tukey")
Tuk

library(palmerpenguins)
library(tidyverse)
datain<-penguins %>% select(flipper_length_mm, species, sex)
datain<-na.omit(datain)


library(dplyr)
# Normality test for each group
datain %>%
  group_by(species, sex) %>%
  summarise(n=n(),
            statistic = shapiro.test(flipper_length_mm)$statistic,
            p.value = shapiro.test(flipper_length_mm)$p.value)

#Homogeneity of Variance
library(car)
leveneTest(flipper_length_mm ~ species*sex , data = datain)


model = lm(flipper_length_mm ~ species*sex, data = datain)
aovtype <- Anova(model, type="III")
aovtype





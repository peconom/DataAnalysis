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

boxplot(formula=Sprint~Smoking, data=d1)

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




ToothGrowth$dose <- factor(ToothGrowth$dose, 
                           levels = c(0.5, 1, 2),
                           labels = c("D0.5", "D1", "D2")
                           )

library("ggpubr")
ggboxplot(ToothGrowth, x = "dose", y = "len", color = "supp")

ibrary(dplyr)
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

library(stats)
with(ToothGrowth, {
  interaction.plot(dose, supp, len, fixed = TRUE)
  interaction.plot(dose, supp, len, fixed = TRUE, col = 2:3, leg.bty = "o")
  interaction.plot(dose, supp, len, fixed = TRUE, col = 2:3, type = "b", main="Interaction plot")
})


library(lsmeans)
Tuk<-lsmeans(aov.res, list(pairwise~dose|supp,pairwise~supp|dose,pairwise~dose+supp),  adjust="tukey")
Tuk


plot(Tuk[[1]])
plot(Tuk[[2]])  
plot(Tuk[[3]])  
plot(Tuk[[4]])  
plot(Tuk[[5]])  
plot(Tuk[[6]]) 

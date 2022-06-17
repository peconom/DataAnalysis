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


library(stats)
with(datain, {
  interaction.plot(species, sex, flipper_length_mm, fixed = TRUE)
  interaction.plot(species, sex, flipper_length_mm, fixed = TRUE, col = 2:3, leg.bty = "o")
  interaction.plot(species, sex, flipper_length_mm, fixed = TRUE, col = 2:3, type = "b", main="Interaction plot")
})
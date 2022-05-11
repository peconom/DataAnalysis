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
leveneTest(score ~ education_level*gender, data = jobsatisfaction)


model = lm(score ~ education_level + gender +  education_level:gender, data = jobsatisfaction)
aov.III <- Anova(model, type="III")
aov.III



library(stats)
with(jobsatisfaction, {
  interaction.plot(education_level, gender, score, fixed = TRUE)
  interaction.plot(education_level, gender, score, fixed = TRUE, col = 2:3, leg.bty = "o")
  interaction.plot(education_level, gender, score, fixed = TRUE, col = 2:3, type = "b", main="Interaction plot")
})
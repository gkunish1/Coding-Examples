setwd("/Users/gavin-kunish/Desktop")
Cereal <- read.csv(file = "Cereal.csv", header = TRUE)

summary(Cereal)


Rating_DF <- data.frame(Cereal$rating, Cereal$protein, Cereal$sodium, Cereal$fiber, Cereal$sugars, Cereal$vitamins, Cereal$weight, Cereal$cups)

summary(Rating_DF)
## Sugars contain missing values

Rating_DF$Cereal.sugars

new_DF <- Rating_DF[-c(58),]
new_DF <- Rating_DF[-c(58,2)]

summary(new_DF)
sd(new_DF$Cereal.rating)
sd(new_DF$Cereal.protein)
sd(new_DF$Cereal.sodium)
sd(new_DF$Cereal.fiber)
sd(new_DF$Cereal.sugar)
sd(new_DF$Cereal.vitamins)
sd(new_DF$Cereal.weight)
sd(new_DF$Cereal.cups)


par(mfrow = c(1,1))

boxplot(new_DF, main = "Figure 1")
# (Figure 1)


cereal_lm <- lm(new_DF$Cereal.rating ~ ., data = new_DF)
cereal_lm

summary(cereal_lm)

plot(new_DF$Cereal.protein, cereal_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.sodium, cereal_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.fiber, cereal_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.sugar, cereal_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.vitamins, cereal_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.weight, cereal_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.cups, cereal_lm$residuals)
abline(h=0)



par(mfrow = c(2,2))
plot(cereal_lm)






library(lmtest)



dwtest(cereal_lm)


qqnorm(cereal_lm$residuals)
qqline(cereal_lm$residuals)
# (Figure 4)
cereal_lm$residuals
cereal_lm$residuals <- cereal_lm$residuals[! cereal_lm$residuals%in% -16.15090486]

shapiro.test(cereal_lm$residuals)


library(MASS)

cereal_trans <- boxcox(new_DF$Cereal.rating ~ ., data = new_DF, plotit = T, lambda = seq(-2,2, by=.5))

cereal_trans

# (Figure 6)

# If 1 is in the region, we dont actually need to do a transformation.

maxyentry <- which.max(cereal_trans$y)
maxyentry
cereal_trans$x[maxyentry]
# 0.9292929




shapiro.test(cereal2_lm$residuals)


dwtest(cereal2_lm)




library(car)
vif(cereal_lm)
# No values above 10, no evidence of multicoliniarity

anova(cereal_lm)
summary(cereal_lm)

cereal3_lm <- step(cereal_lm, data = new_DF, direction = "backward")
summary(cereal3_lm)

qqnorm(cereal3_lm$residuals)
qqline(cereal3_lm$residuals)

shapiro.test(cereal3_lm$residuals)


dwtest(cereal3_lm)


plot(new_DF$Cereal.protein, cereal3_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.sodium, cereal3_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.fiber, cereal3_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.sugar, cereal3_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.vitamins, cereal3_lm$residuals)
abline(h=0)
plot(new_DF$Cereal.cups, cereal3_lm$residuals)
abline(h=0)

plot(cereal3_lm)

summary(cereal3_lm)
anova(cereal3_lm)

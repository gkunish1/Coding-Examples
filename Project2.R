### Project 2

#Task 1
LifeExpectancy <- read.csv(file = "/Users/gavin-kunish/Desktop/LifeExpectancy.csv")

LifeExpectancy$status <- factor(LifeExpectancy$status)
LifeExpectancy$continent <- factor(LifeExpectancy$continent)
LifeExpectancy$sub_region <- factor(LifeExpectancy$sub_region)

LifeExpectancy$thin_youth <- ordered(LifeExpectancy$thin_youth, levels = c("Low", "Medium", "High"))
LifeExpectancy$thin_child <- ordered(LifeExpectancy$thin_child, levels = c("Low", "Medium", "High"))
LifeExpectancy$mortality_level <- ordered(LifeExpectancy$mortality_level, levels = c("Low", "Moderate", "High"))

#Task 2
# Ho: mu = 250
# H1: mu > 250

t.test(LifeExpectancy$measles, mu = 250, alternative = "greater", conf.level = .94)

# One Sample t-test
# 
# data:  LifeExpectancy$measles
# t = 2.0317, df = 170, p-value = 0.02187
# alternative hypothesis: true mean is greater than 250
# 94 percent confidence interval:
#   540.1701      Inf
# sample estimates:
#   mean of x 
# 1506.789 



# We reject the null hypothesis at a 6% significance level
# Thus we can say there is enough evidence to say the true mean is greater than 250

# The true mean of 540 is greater than our hypothesized value of 250, supporting our conclusion

# We can use the central limit theorem because of the large sample size

## Task 3

Developed_Schooling <- LifeExpectancy[LifeExpectancy$status == "Developed", 10]

Developing_Schooling <- LifeExpectancy[LifeExpectancy$status == "Developing", 10]

var.test(Developed_Schooling, Developing_Schooling, ratio = 1)

# Since the p-value is greater than the significance level, we can conclude they are of equal variances

t.test(Developed_Schooling, Developing_Schooling, mu = 0, conf.level = .99, var.equal = TRUE)

# Ho: mu_Developed = mu_Developing
# H1: mu_Developed != mu_Developing

# Decision: Reject the null hypothesis

# Conclusion: There is enough evidence to say there is a difference of means between Schooling in Developed and Developing Countries

# We are 99% confident that the true difference of means is between 3 and 5.49


## Task 4

Mortality_Youth <- table(LifeExpectancy$mortality_level, LifeExpectancy$thin_youth)


chisq.test(Mortality_Youth, correct = FALSE)

# Ho: Mortality_level and Thin_youth are independed
# H1: Mortality_level and thin_youth are not independent

# Decision: Reject the null hypothesis at a 2% significance level

# Conclustion: There is enough evidence to say that Mortality_level and thin_youth are not independent

## Task 5

lm_IncomeComp <- lm(LifeExpectancy$life.expectancy ~ LifeExpectancy$income.composition.of.resources)

plot(x = LifeExpectancy$income.composition.of.resources, y = lm_IncomeComp$residuals)
# It appears that the linearity test is not passed

qqnorm(lm_IncomeComp$residuals)
qqline(lm_IncomeComp$residuals)
# It appears normality is not met

plot(lm_IncomeComp)
# It looks like equal variance is not met

summary(lm_IncomeComp)

# Ho: Beta1 = 0
# H1: Beta1 != 0

# p-value = 2.2e-16

# Decision: Reject the null hypothesis at a 1% significance level

# Conclusion: There is enough evidence of a significant linear relationship between Life Expectance and Income composition of resources

# Yhat_i = 39.26 + 46.91*x_i

# R^2 = .8195

# 81.95% of the variability in Life Expectancy can be explained by Income composition of resources

# residual standard error ?

## Task 6

lm_LifeExpectancy <- lm(LifeExpectancy$life.expectancy ~ LifeExpectancy$infant.deaths + LifeExpectancy$BMI + LifeExpectancy$measles)

plot(x = LifeExpectancy$infant.deaths, y = lm_LifeExpectancy$residuals)
# Appears to pass the linearity check

plot(x = LifeExpectancy$BMI, y = lm_LifeExpectancy$residuals)
# Does not appear to pass the linearity check

plot(x = LifeExpectancy$measles, y = lm_LifeExpectancy$residuals)
# Appears to pass the linearity check

qqnorm(lm_LifeExpectancy$residuals)
qqline(lm_LifeExpectancy$residuals)
# This may be from a normal distribution, problems at the bottom and top of the graph

par(mfrow = c(2,2))
plot(lm_LifeExpectancy)
par(mfrow = c(1,1))

# It looks like equal variance is not met since it strays too far from the reference line

# Ho: All Beta_j are equal to 0 where j= 1,2,3
# H1: At least 1 Beta_j != 0

summary(lm_LifeExpectancy)

# Call:
#   lm(formula = LifeExpectancy$life.expectancy ~ LifeExpectancy$infant.deaths + 
#        LifeExpectancy$BMI + LifeExpectancy$measles)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -17.3490  -3.9195   0.5299   4.6389  22.3656 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                  64.5468374  1.2336965  52.320  < 2e-16 ***
#   LifeExpectancy$infant.deaths -0.0312127  0.0105749  -2.952  0.00362 ** 
#   LifeExpectancy$BMI            0.1775219  0.0253086   7.014 5.49e-11 ***
#   LifeExpectancy$measles        0.0002591  0.0001092   2.374  0.01874 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.688 on 167 degrees of freedom
# Multiple R-squared:  0.2924,	Adjusted R-squared:  0.2797 
# F-statistic: 23.01 on 3 and 167 DF,  p-value: 1.624e-12


# F = 23.01

# p-value = 1.624e-12

# Decision: Reject the null hypothesis at a 1% significance level

# Conclusion: There is evidence that the x variables Infant Deaths, BMI, and measles explain some of the variability of Life Expectancy

# R^2 = .2924
# R^2_adj = .2797

# Beta_Infant.deaths

# Ho: Beta_Infant.deaths = 0
# H1: Beta_Infant.deaths != 0

# t = -.0312

# p-value = .00362

# Decision: Reject the null hypothesis at a 1% significance level

# Conclusion: There is enough evidence to say that Infant.deaths is important in explaining the variance of Life Expectancy

# Beta_BMI

# Ho: Beta_BMI = 0
# H1: Beta_BMI != 0

# t = .1776

# p-value = 5.49e-11

# Decision: Reject the null hypothesis at a 1% significance level

# Conclusion: There is enough evidence to say that BMI is important in explaining the variance of Life Expectancy

# Beta_Measles

#H0: Beta_Measles = 0
#H1: Beta_Measles != 0

# t = .00025

# p-value = .018

# Decision: Do not reject the null hypothesis

# Conclusion: There is not enough evidence to say that Measles is important in explaining the variance of Life Expectancy


## Task 7

lm_measles <- lm(LifeExpectancy$measles~LifeExpectancy$continent)

hist(lm_measles$residuals)

# Very skewed right, probably not normal

qqnorm(lm_measles$residuals)
qqline(lm_measles$residuals)

# Lots of outliers, probably not normal

shapiro.test(lm_measles$residuals)

# Shapiro-Wilk normality test
# 
# data:  lm_measles$residuals
# W = 0.30152, p-value < 2.2e-16

# Very low p-value, probably not normal

plot(lm_measles$fitted.values, lm_measles$residuals)

# There are large differences of the vertical spread, data might not have equal variance

boxplot(lm_measles$residuals, LifeExpectancy$continent)
# Large differences in range of boxplots, data might not have equal variance

# Ho: The variance of all the continents are the same
# H1: The variance at least 1 continent are different

bartlett.test(LifeExpectancy$measles~LifeExpectancy$continent)

# Bartlett test of homogeneity of variances
# 
# data:  LifeExpectancy$measles by LifeExpectancy$continent
# Bartlett's K-squared = 627.57, df = 4, p-value < 2.2e-16

# p-value = 2.2e-16

# Decision: Reject the null hypothesis

# Conclusion: There is enough evidence to say that at least one of the variances of continent are different

aov_measles <- aov(lm_measles)
summary(aov_measles)

# Df    Sum Sq   Mean Sq F value Pr(>F)  
# LifeExpectancy$continent   4 5.419e+08 135485745   2.125 0.0799 .
# Residuals                166 1.058e+10  63744928                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Since the p-value of .0799 is greater than our signifiance level we can assume there are probably not significant differences in Continent

## Task 8

residual <- lm_under.five$residuals
lm_under.five_2 <- cbind(LifeExpectancy, residual)


under.5_2A <- lm_under.five_2$residual[lm_under.five_2$thin_child == "Low"]
under.5_2B <- lm_under.five_2$residual[lm_under.five_2$thin_child == "Medium"]
under.5_2C <- lm_under.five_2$residual[lm_under.five_2$thin_child == "High"]

lm_under.five <- lm(LifeExpectancy$under.five.deaths ~ LifeExpectancy$thin_child)

hist(lm_under.five$residuals)
# Very skewed right, probably not normal

qqnorm(lm_under.five$residuals)
qqline(lm_under.five$residuals)

# Lots of outliers, probably not normal

shapiro.test(lm_under.five$residuals)

# Shapiro-Wilk normality test
# 
# data:  lm_under.five$residuals
# W = 0.31044, p-value < 2.2e-16

# Small p-value, probably not normal

plot(lm_under.five$fitted.values, lm_under.five$residuals)

# Large differences in vertical spread, data might not have equal variance

boxplot(lm_under.five$residuals, LifeExpectancy$thin_child)

# Large differences in boxplot range, data might not have equal variance


# Ho: All the variances of thin_child are the same
# H1: At least one variance in thin_child is different
bartlett.test(LifeExpectancy$under.five.deaths ~ LifeExpectancy$thin_child)

# Bartlett test of homogeneity of variances
# 
# data:  LifeExpectancy$under.five.deaths by LifeExpectancy$thin_child
# Bartlett's K-squared = 185.15, df = 2, p-value < 2.2e-16

# Decision: Reject the null hypothesis

# Conclusion: There is enough evidence to say that at least one of the variances in thin_child is different
library(car)
leveneTest(LifeExpectancy$under.five.deaths ~ LifeExpectancy$thin_child)

aov_under.five <- aov(lm_under.five)
summary(aov_under.five)

# Df  Sum Sq Mean Sq F value Pr(>F)  
# LifeExpectancy$thin_child   2   94921   47461   4.147 0.0175 *
#   Residuals                 168 1922743   11445                 
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Since the p-value of .0175 is less than our significance level of 6% we can assume that there probably are significant differences in thin_child

TukeyHSD(aov_under.five, conf.level = .96)


# Tukey multiple comparisons of means
# 96% family-wise confidence level
# 
# Fit: aov(formula = lm_under.five)
# 
# $`LifeExpectancy$thin_child`
# diff        lwr      upr     p adj
# Medium-Low   2.880781 -49.796585 55.55815 0.9901229
# High-Low    49.117794   2.285812 95.94978 0.0292877
# High-Medium 46.237013  -4.264478 96.73850 0.0664710

# Medium-Low: Not a significant difference
# High-Low: Significant difference
# High-Medium: Not a significant difference

## Task 9
LifeExpectancy$continent <- factor(LifeExpectancy$continent)
LifeExpectancy$mortality_level <- factor(LifeExpectancy$mortality_level)

lm_LE <- lm(LifeExpectancy$life.expectancy ~ LifeExpectancy$continent * LifeExpectancy$mortality_level)


hist(lm_LE$residuals)
# Fairly skewed, probably not normal

qqnorm(lm_LE$residuals)
qqline(lm_LE$residuals)

# Lots of outliers, probably not normal

shapiro.test(lm_LE$residuals)

# Shapiro-Wilk normality test
# 
# data:  lm_LE$residuals
# W = 0.93661, p-value = 7.247e-07

# Very small value, probably not normal 

par(mfrow = c(2,2))
plot(lm_LE)
par(mfrow = c(1,1))

# The spreads appear to be about the same, the equal variance assumption is likely met

# Ho: (alphaBeta)_ij = 0
# H1: at least 1 (alphaBeta)_ij != 0

summary.aov(lm_LE)

# Df Sum Sq Mean Sq
# LifeExpectancy$continent                                  4   5903  1475.9
# LifeExpectancy$mortality_level                            2   1675   837.3
# LifeExpectancy$continent:LifeExpectancy$mortality_level   8    617    77.1
# Residuals                                               156   2363    15.1
# F value   Pr(>F)    
# LifeExpectancy$continent                                 97.446  < 2e-16 ***
#   LifeExpectancy$mortality_level                           55.282  < 2e-16 ***
#   LifeExpectancy$continent:LifeExpectancy$mortality_level   5.089 1.23e-05 ***
#   Residuals                                                                   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# p-value = 1.23e-05

# Decision: Reject the null hypothesis

# Conclusion: There is enough evidence to say that the interaction effect is present


## Task 10

LifeExpectancy$thin_youth <- factor(LifeExpectancy$thin_youth)
LifeExpectancy$status <- factor(LifeExpectancy$status)

lm_LE2 <- lm(LifeExpectancy$life.expectancy ~ LifeExpectancy$thin_youth * LifeExpectancy$status)


hist(lm_LE2$residuals)
# The histogram looks to be relatively symmetrical, could be normal

qqnorm(lm_LE2$residuals)
qqline(lm_LE2$residuals)
# Appear to be some outliers, may not be normal

shapiro.test(lm_LE2$residuals)

# Shapiro-Wilk normality test
# 
# data:  lm_LE2$residuals
# W = 0.98309, p-value = 0.03572

# p-value is greater than our significance level, we can assume the data is normal

par(mfrow = c(2,2))
plot(lm_LE2)
par(mfrow = c(1,1))

# The spread appears to be about the same, equal variance is likely met

# Ho: (alphaBeta)_ij = 0
# H1: at least 1 (alphaBeta)_ij != 0


summary.aov(lm_LE2)

# p-value = .701

# Decision: Fail to reject the null hypothesis at 3% significance

# Conclusion: There is not enough evidence to say that the interaction effect is present





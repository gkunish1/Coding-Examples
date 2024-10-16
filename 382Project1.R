##Project 1

#Task 1
Countries <- read.csv(file = "/Users/gavin-kunish/Desktop/Countries.csv")
summary(Countries)

Countries$status <- factor(Countries$status)

Countries$continent <- factor(Countries$continent)

Countries$thin_youth <- ordered(Countries$thin_youth, levels = c("Low", "Medium", "High"))
summary(Countries$thin_youth)

Countries$thin_child <- ordered(Countries$thin_child, levels = c("Low", "Medium", "High"))


#Task 2
summary(Countries$schooling)
is.na(Countries$schooling)

hist(Countries$schooling, main = "Schooling")

boxplot(Countries$schooling, main = "Schooling")

mean(Countries$schooling)
# 13.0076
sd(Countries$schooling)
# 2.828989

#Task 3
summary(Countries$thin_youth)
barplot(table(Countries$thin_youth), main = "Thin Youth")

table(Countries$thin_youth)

# 1

#Task 4
boxplot(Countries$schooling~Countries$status)
tapply(Countries$schooling, Countries$status, summary)

# Median


#Task 5
boxplot(Countries$measles~Countries$continent, ylim = c(0,2000))
tapply(Countries$measles, Countries$continent, summary)



#Task 6
hist(Countries$schooling, right = T)
hist(Countries$measles, right = T, xlim = c(0,20000))

qqnorm(Countries$schooling)
qqline(Countries$schooling)

qqnorm(Countries$measles)
qqline(Countries$measles)

skewness(Countries$schooling)
skewness(Countries$measles)

kurtosis(Countries$schooling)
kurtosis(Countries$measles)

shapiro.test(Countries$schooling)
shapiro.test(Countries$measles)

#Task 7
Countries$mortality_level[Countries$adult.mortality < 80] <- "Low"
Countries$mortality_level[Countries$adult.mortality >= 80 & Countries$adult.mortality < 150] <- "Moderate"
Countries$mortality_level[Countries$adult.mortality >= 150] <- "High"

Countries$mortality_level <- ordered(Countries$mortality_level, levels = c("Low", "Moderate", "High"))

boxplot(Countries$mortality_level~Countries$thin_youth)




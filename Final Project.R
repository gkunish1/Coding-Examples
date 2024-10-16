##### Final Project
##### Name: Gavin Kunishsurvey <- read.csv(file = "/Users/gavin-kunish/Desktop/Student_Survey_Dataset.csv")
##### Version Number: 1




## Task 1: Load the Dataset into R (Code)
dataset <- read.csv(file = "/Users/gavin-kunish/Desktop/28012288.csv")





## Task 2: Summary Statistics for App_Size
# a) Summary Stats Code and Results
App_Size <- dataset$App_Size
summary(App_Size)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
0.029   8.450  26.000  30.902  48.000  99.000 


# b) Variance Code and Results
var(App_Size)
[1] 717.7246


# c) Standard Deviation Code and Results
sd(App_Size)
[1] 26.79038


# d) IQR Code and Results
IQR(App_Size)
[1] 39.55


# e) Number of values for this variable. Code and Results
length(App_Size)
[1] 164


# f) Mean / Median Comparison: 
The mean is larger than the median


# g) SD / IQR comparison: 
The IQR is larger than the SD







## Task 3: Summary Statistics for Rating
# a) Summary Stats Code and Results
Rating <- dataset$Rating
summary(Rating)
Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
3.8     4.2     4.3     4.3     4.4     4.8 


# b) Variance Code and Results
var(Rating)
[1] 0.04134969


# c) Standard Deviation Code and Results
sd(Rating)
[1] 0.2033462


# d) IQR Code and Results
IQR(Rating)
[1] 0.2


# e) Number of values for this variable. Code and Results
length(Rating)
[1] 164


# f) Mean / Median Comparison: 
Mean and median are the same


# g) SD / IQR comparison: 
IQR and SD are extermely similar







## Task 4: Histogram for App_Size
## Remember to save your plot and also submit it to Gradescope.

# Code:
hist(App_Size, right = FALSE,breaks = seq(0,100, 20), ylim = c(0,80))


# Describe histogram: relatively symmetric, skewed, or neither?
It is skewed right


# Describe histogram: unimodal or bimodal or multimodal?
It is unimodal







## Task 5: Boxplot for App_Size
## Remember to save your plot and also submit it to Gradescope.

# Code:
Box <- boxplot(App_Size, horizontal = F, ylim = c(-10,100), main = "Boxplot of App Size")


# Are there outliers present?

Box$out
numeric(0)

No there are no outliers






## Task 6: Shapiro-Wilk Test for App_Size using 0.01 as the cutoff
# Code:
shapiro.test(App_Size)


# Copy and paste results here

data:  App_Size
W = 0.89667, p-value = 2.633e-09


# Do you think that your population is normally distributed?
it is not normally distributed


# Reason:

Since the p value is < .01 

# Does your decision here match what you are seeing with your histogram from Task 4? Why or why not?

Yes, the histogram was skewed thus not normal






## Task 7: Histogram for Rating
## Remember to save your plot and also submit it to Gradescope.

# Code:
hist(Rating, right = FALSE,breaks = seq(3.8,5, .1), ylim = c(0,35))


# Describe histogram: relatively symmetric, skewed, or neither?
It is relatively symmetric


# Describe histogram: unimodal or bimodal or multimodal?
It is unimodal







## Task 8: Boxplot for Rating
## Remember to save your plot and also submit it to Gradescope.

# Code:
Box1 <- boxplot(Rating, horizontal = F, ylim = c(3.7,5), main = "Boxplot of Rating")


# Are there outliers present?
Box1$out
[1] 3.8 3.8 4.8 4.8

Yes







## Task 9: Shapiro-Wilk Test for Rating using 0.01 as the cutoff
# Code:
shapiro.test(Rating)


# Copy and paste results here

Shapiro-Wilk normality test

data:  Rating
W = 0.97804, p-value = 0.01041


# Do you think that your population is normally distributed?
Yes


# Reason:
The p-value is > .01


# Does your decision here match what you are seeing with your histogram from Task 7? Why or why not?

Yes, the histogram is bell-shaped meaning normally distributed






## Task 10: Compare average Reviews for different Categories. Create a confidence interval, assuming equal variances. (See PDF for CI to create.)

# a) Split Dataset:
GAME <- dataset$Reviews[dataset$Category == "GAME"]
NotGAME <- dataset$Reviews[dataset$Category != "GAME"]


# b) Code:
t.test(GAME,NotGAME, alternative = "two.sided", conf.level = .934, var.equal = TRUE)


# c) Copy and paste results here
Two Sample t-test

data:  GAME and NotGAME
t = 2.5169, df = 162, p-value = 0.01281
alternative hypothesis: true difference in means is not equal to 0
93.4 percent confidence interval:
  97413.27 638880.97
sample estimates:
  mean of x mean of y 
480220.9  112073.8 


# d) State the parameter the confidence interval is for.
mu_GAME - mu_NotGAME


# e) Write down the confidence interval.

97,413.27 to 638,880.97

# f) Write an interpretation of your confidence interval.
We are 94.3% confident the true differences of mean GAME - mean NotGame will be between 97,413.27 and 638,880.97


# g) We are interested in whether there is evidence that there is specific value difference between the two groups. (See PDF for more specifics.)
# Does this value seem plausible?
No

# Reason why or why not.

It falls outside of our confidence interval






## Task 11: Create a confidence interval for the proportion an app's Content Ratings that are a specific one.

# a) Code to observe how many values exist per Content Rating.
addmargins(table(datasetname$Content.Rating))



# b) Check the success / failure condition.
# Code:
164*0.7195122
164*(1-0.7195122)

# Expected Number of Successes = 118
# Expected Number of Failures = 46
# Can we use the Normal Distribution to approximate this confidence interval?
Yes

# c) Code for Confidence Interval:
prop.test(118,164, alternative = "two.sided", conf.level = .911, correct = FALSE)


# d) Copy and paste results here

1-sample proportions test without continuity correction

data:  118 out of 164, null probability 0.5
X-squared = 31.61, df = 1, p-value = 1.885e-08
alternative hypothesis: true p is not equal to 0.5
91.1 percent confidence interval:
  0.6564452 0.7749706
sample estimates:
  p 
0.7195122 


# e) State the parameter the confidence interval is for.
Population proportion of E rated games


# f) Write down the confidence interval.
0.6564452 to 0.7749706







## Task 12: Create a confidence interval for the variance of Rating. (See PDF for CI to create.)

# a) Code:
library(EnvStats)
varTest(Rating, alternative = "two.sided", conf.level = .988)


# b) Copy and paste results here
$statistic
Chi-Squared 
6.74 

$parameters
df 
163 

$p.value
[1] 1.373853e-80

$estimate
variance 
0.04134969 

$null.value
variance 
1 

$alternative
[1] "two.sided"

$method
[1] "Chi-Squared Test on Variance"

$data.name
[1] "Rating"

$conf.int
LCL        UCL 
0.03180991 0.05561583 
attr(,"conf.level")
[1] 0.988

attr(,"class")
[1] "htestEnvStats"


# c) State the parameter the confidence interval is for.
Sample variance of Rating


# d) Write down the confidence interval.
0.03180991 to 0.05561583


# e) What assumption did we need to make to construct this confidence interval?
We needed to assume the population is normal


# e) Do you think that this assumption was met? You should reference an earlier Task from this project to answer this question.


yes since the hisogram of Rating was symmetric it implies normality





## Task 13: Hypothesis Test for App_Size
# a) What conditions must you satisfy to perform this test? Do you think they are met?  Why or why not?
The Shapiro-Wilkes test showed the population was not normal however we still must assume normality. Since n=164 ("large") we can assume normality throught the CLT.



# b) State the hypotheses:
#H0:u=27.35
#H1:u>27.35


# c) Code:
t.test(App_Size, mu = 27.35, alternative = "greater", conf.level = .958)


# d) Copy and paste results here
One Sample t-test

data:  App_Size
t = 1.6981, df = 163, p-value = 0.0457
alternative hypothesis: true mean is greater than 27.35
95.8 percent confidence interval:
  27.26535      Inf
sample estimates:
  mean of x 
30.90238 


# e) State the Test Statistic Value:
t = 1.6981

# f) State the P-Value:
p-value = .0457

# g) Decision from P-Value (see PDF for significance level):
.0457 !< .042 so we fail to reject the null hypothesis

# h) Conclusion from P-Value:
There is enough evidence to support the claim that the true population mean of App Size is greater than 27.35 MB


# i) State the critical value. Provide your code and results.
qt(.042, 163, lower.tail = F)
[1] 1.738562


# i) State the critical region.
Reject the null hypothesis if T > 1.738562


# j) Would you make the same decision based on the critical region that you did with your p-value? Why or why not?
Yes we would make the same decision. 1.698 !> 1.739







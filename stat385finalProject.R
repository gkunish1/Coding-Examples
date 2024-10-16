setwd("/Users/gavin-kunish/Desktop")
CPS <- read.csv(file = "CPS_Data.csv", header = TRUE)

summary(CPS)

## Turning character variables into numeric values

CPS$Family.Involvement.Score
CPS$Family.Involvement.Score <- as.numeric(CPS$Family.Involvement.Score)

CPS$Leaders.Score <- as.numeric(CPS$Leaders.Score)

CPS$Teachers.Score <- as.numeric(CPS$Teachers.Score)

CPS$Parent.Engagement.Score <- as.numeric(CPS$Parent.Engagement.Score)

CPS$Graduation.Rate.. <- as.numeric(CPS$Graduation.Rate..)

## Creating a new data frame with just the data were using then cleaning

new.DF <- data.frame(CPS$Graduation.Rate.., CPS$Safety.Score, CPS$Family.Involvement.Score,
                     CPS$Environment.Score, CPS$Instruction.Score, CPS$Leaders.Score, 
                     CPS$Teachers.Score, CPS$Parent.Engagement.Score, CPS$Average.Student.Attendance, 
                     CPS$Average.Teacher.Attendance)
new.DF <- na.omit(new.DF)
new.DF$CPS.Graduation.Rate..
names(new.DF)

## Linear model on our data with Graduation Rate as our Response variable

lm.test <- lm(new.DF$CPS.Graduation.Rate.. ~., data = new.DF)
summary(lm.test)

lm.prediction <- predict(lm.test, test.data)
lm.mse <- mean((lm.prediction - test.data$CPS.Graduation.Rate..)^2)
lm.mse

## Stepwise AIC selection

library(MASS)
stepmod=lm(new.DF$CPS.Graduation.Rate.. ~., data = new.DF)
AICmod=stepAIC(stepmod,direction="both",trace=1)
summary(AICmod)

lm.prediction2 <- predict(stepmod, test.data)
lm.mse3 <- mean((lm.prediction2 - test.data$CPS.Graduation.Rate..)^2)
lm.mse3

# Ridge Regression
install.packages("glmnet")
library(glmnet)
x <- model.matrix(CPS.Graduation.Rate.. ~ ., data = new.DF)[, -1]
y <- new.DF$CPS.Graduation.Rate..

#xmat.project <- as.matrix(new.DF[,-1])

set.seed(1)
sample.project <- sample(1:nrow(new.DF),size=round(0.7*nrow(new.DF)))

trainset.project <- new.DF[sample.project, ]
testset.project2 <- c(1:nrow(new.DF))[-sample.project]

test.data <- new.DF[testset.project2, ]



# Use cross-validation to find the optimal lambda
cv_ridge <- cv.glmnet(x, y, subset = trainset.project, alpha = 0, nfolds = 5)
optimal_lambda_ridge <- cv_ridge$lambda.min

# Fit the ridge regression model with the optimal lambda
ridge_model <- glmnet(x, y, subset = trainset.project, alpha = 0, lambda = optimal_lambda_ridge)
coef(ridge_model, s = optimal_lambda_ridge)

ridge.prediction <- predict(ridge_model, s = optimal_lambda_ridge, newx = x[testset.project2, ])

ridge.mse <- mean((ridge.prediction - test.data$CPS.Graduation.Rate..)^2)
ridge.mse

# Lasso Regression
# Use cross-validation to find the optimal lambda
cv_lasso <- cv.glmnet(x, y, subset = trainset.project, alpha = 1, nfolds = 5)
optimal_lambda_lasso <- cv_lasso$lambda.min

# Fit the lasso regression model with the optimal lambda
lasso_model <- glmnet(x, y, alpha = 1, subset = trainset.project, lambda = optimal_lambda_lasso)
coef(lasso_model, s = optimal_lambda_lasso)

lasso.prediction <- predict(lasso_model, s = optimal_lambda_lasso, newx = x[testset.project2, ])
lasso.prediction

lasso.mse <- mean((lasso.prediction - test.data$CPS.Graduation.Rate..)^2)
lasso.mse

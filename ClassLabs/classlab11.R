#######################################
#                                     #
#    Classification: Debt Repayment   #
#                                     #
#              Class 11               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   Regression:
#   - Make predictions
#   - Calculate the accuracy of the model on test and training
#   - OLS Diagnostics
#   Classification
#   - Why is linear regression not appropriate?
#   - Estimating logit model using glm()
#   - Odds ratio interpretation
#   - Exponentiating logit coefficients to understand impact on odds ratio
#   - Generating individual predictions from estimated logit model
#   - "Scoring" dataset 
#   - Multiple logistic regression
#######################################

#--------------------------------------
# 1. OLS prediction and diagnostics
#--------------------------------------
# load data "Auto" from ISLR package
library(ISLR)
Auto$origin <- factor(Auto$origin, labels = c("American","European","Japanese"))

# split data into training and test
set.seed(310)
train_idx <- sample(1:nrow(Auto),size = 0.8*nrow(Auto),replace=FALSE)
Auto_train <- Auto[train_idx,]
Auto_test <- Auto[-train_idx,]

# model1: mpg ~ horsepower + cylinders + weight + origin
model1 <- lm(mpg ~ horsepower + cylinders + weight + origin, Auto_train)
summary(model1)

# model predictions
preds_train1 <- predict(model1)

head(preds_train1)

preds_train1_df <- data.frame(true = Auto_train$mpg,
                               pred = preds_train1,
                               resid = model1$residuals)

preds_test1_df <- data.frame(true = Auto_test$mpg,
                             pred = predict(model1, newdata = Auto_test))
# model accuracy (error)
# using caret package
library(caret)

# training error
RMSE(preds_train1_df$pred, preds_train1_df$true)
# test error
RMSE(preds_test1_df$pred, preds_test1_df$true)

# calculating RMSE by hand
resid_test <- preds_test1_df$true - preds_test1_df$pred
sqrt(mean(resid_test^2))
# model2: mpg ~ horsepower + horsepower^2 + cylinders + weight + origin
model2 <- lm(mpg ~ horsepower + I(horsepower^2) + cylinders + weight + origin, Auto_train)
summary(model2)

# predictions and accuracy
preds_train2_df <- data.frame(true = Auto_train$mpg,
                              pred = predict(model2),
                              resid = model2$residuals)

preds_test2_df <- data.frame(true = Auto_test$mpg,
                              pred = predict(model2, newdata = Auto_test))
                              

# training
RMSE(preds_train2_df$pred, preds_train2_df$true)
# test
RMSE(preds_test2_df$pred, preds_test2_df$true)

# model3: mpg ~ all variables except name
model3 <- lm(mpg ~ . -name, Auto_train)
summary(model3)

# heteroskedasticity
library(ggplot2)

ggplot(preds_train1_df, aes(pred, resid)) +
  geom_point() + 
  geom_smooth(se = FALSE)

# collinearity
install.packages("olsrr")
library("olsrr")

ols_vif_tol(model1)

ols_vif_tol(model2)



#--------------------------------------
# 2. Classification Data
#--------------------------------------
# load data "Default" from ISLR package
data("Default")

head(Default)

# we can force R to avoid scientif number formats

# create a binary (dummy) variable for default (outcome)
Default$default_binary <- ifelse(Default$default == "Yes", 1, 0)

table(Default$default, Default$default_binary)
summary(Default)
#--------------------------------------
# 3. Linear Model
#--------------------------------------
# estimate an OLS model using the 0,1 
# variable as our dependent variable
ols1 <- lm(default_binary ~ balance, Default)

summary(ols1)
# predicted outcome (create a dataframe with other data)
preds_ols_df <- data.frame(true = Default$default_binary,
                           pred = predict(ols1))

# what kind of predictions do we get for this model?


# plotting actual outcome with the fitted linear line
ggplot(Default, aes(balance, default_binary)) +
  geom_point() +
  geom_abline(intercept = ols1$coefficients[1],
              slope = ols1$coefficients[2],
              color = "red")

#--------------------------------------
# 4. Logistic Model
#--------------------------------------

# logistic model default ~ balance 
logit1 <- glm(default ~ balance, family = binomial,
              data = Default)

summary(logit1)

# interpretation

# display exponentiated coefficients (optional)
# install.packages("jtools")



#--------------------------------------
# 5. Prediction Probabilities
#--------------------------------------

# predicted probabilities (manually)
# @ balance == 1000

# recall: Pr(Y=1|X=1000) = exp(beta_0 + beta_1 * X) / (1 + exp(beta_0 + beta_1 * X))


# predicting for all the rows
# score is the predicted probability
score <- predict(logit1, type = "response")

head(score)
# storing scores in a dataframe
?aggregate

# distributions of scores (predicted probs)
# boxplot by default == Yes or No



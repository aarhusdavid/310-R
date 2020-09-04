rm(list = ls()) #removing all variables
library("ISLR")
library("ggplot2")
library("corrplot")
library("caret")
library("tidyverse")
library("leaps")
library("doBy")
library("glmnet")
library("glmnetUtils")

youtube_train <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/youtube_train_A.csv")
youtube_test <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/youtube_test_A.csv")

#Question 2
# Using ggplot, produce a scatter plot of views by the number of followers, 
# faceted by genre of the video. Include a linear trendline for each plot. 
# For which video genres is the relationship 
# between views and followers the strongest?

#code
ggplot(youtube_train, aes(views, followers)) + geom_point() + 
  stat_smooth() + facet_wrap(vars(genre), scales = "free")

#Question 3
# Use summaryBy in the “doBy” package to compare average video views and duration 
# for sponsored and non-sponsored videos. Are sponsored videos lengthier than 
# non-sponsored videos? Which one has more views on average?

#code
summaryBy( views ~ sponsored, youtube_train, FUN = mean)
summaryBy( duration ~ sponsored, youtube_train, FUN = mean)


#Question 4

# Build a linear regression model that predicts total views as a function of 
# video duration, the number of followers, age_18_24, female_percent, and type. 
# Interpret the coefficient on followers. What variables are statistically significant?

model <- lm(views ~ duration 
            + followers 
            + age_18_24
            + female_percent
            + type , youtube_train)
summary(model)

#Question 5
# Predict the number of views and calculate the RMSE (root mean squared error) 
# for the training and test sets. How effective is the model at predicting the
# number of views? Comment on the quality of the model overall. 
# Is the model overfit or underfit, and how do we know?

preds_train1 <- predict(model, newdata = youtube_train)
preds_test1 <- predict(model, newdata = youtube_test)

preds_train1_df <- data.frame(true = youtube_train$views,
                              pred = predict(model, newdata = youtube_train),
                              resid = youtube_train$views - predict(model, newdata =  youtube_train))

preds_test1_df <- data.frame(true = youtube_test$views,
                             pred = predict(model, newdata = youtube_test),
                             resid = youtube_test$views - predict(model, newdata = youtube_test))

# MSE for train and test
RMSE(preds_train1_df$pred, preds_train1_df$true)
RMSE(preds_test1_df$pred, preds_test1_df$true)

#Question 6
# Estimate a forward stepwise model to predict total views using all predictors. 
# Set the maximum number of variables equal to 6. Print the stepwise model and 
# state which variables the model selects at the last step (model with 6 variables).

forward_mod <- regsubsets(views ~ .,
                          data = youtube_train,
                          nvmax = 6,
                          method = "forward")
summary(forward_mod)



#Question 7
# Estimate a Lasso model over the same data (using all the predictors). 
# Print the coefficients using lambda.min and lambda.1se. 
# Print the MSE plot as a function of lambda in Lasso. 
# How many variables does the model select for lambda.1se?

# estimate Lasso mod 
lasso_mod <- cv.glmnet(views ~ .,
                       data = youtube_train,
                       alpha = 1)

# print the coefficients for lambda.min and lambda.1se
lasso_mod$lambda.min
lasso_mod$lambda.1se

## Print the MSE plot as a function of lambda in Lasso. 
plot(lasso_mod)

# put in a matrix
coef(lasso_mod, s = lasso_mod$lambda.1se)

#Question 8
# If the goal is to maximize the exposure of the videos (views), 
# what would you suggest as the best strategy to the content creators? 
# What variables affect the viewership? 
# What would you recommend as to the best model to predict views? 
# How confident would you feel about your best model? 





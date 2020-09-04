#                                     #
#     Classification - Titanic        #
#                                     #
#              Class 14               #
#                                     #
#              MGSC 310               #

rm(list=ls())
#--------------------------------------
# 1. Down-sampling and Up-sampling (Default data set)
#--------------------------------------
library(ISLR)
data(Default)
#install.packages("ROSE")


library(ROSE)
downsampled <- ROSE(default ~., data = Default, 
                       N = 666, p = 1/2)

table(downsampled$data$default)
table(Default$default)

upsampled <- ROSE(default ~., 
                     data = Default, 
                     N = 12000, 
                     p = 1/2)

table(upsampled$data$default)


# vanilla logit
regular_logit <- glm(default ~ .,
             data = Default,
             family = binomial)

down_logit <- glm(default ~ .,
                  data = downsampled$data,
                  family = binomial)

up_logit <- glm(default ~ .,
                data = upsampled$data,
                family = binomial)
# generate scores and class predictions
scores_down = predict(down_logit,
                      type = "response")

scores_up = predict(up_logit,
                    type = "response")

scores_reg = predict(regular_logit,
                     type = "response")

class_down <- ifelse(scores_down > 0.5,1,0)
class_up <- ifelse(scores_up > 0.5,1,0)
class_reg <- ifelse(scores_reg > 0.5,1,0)

table(class_reg, Default$default)

sen_reg <- 105 / (228 + 105)
spe_reg <- 9627 / (9627 + 40)

table(class_down, downsampled$data$default)

sen_down <- 306 / (306 + 39)
spe_down <- 268 / (268 + 53)
  
table(class_up, upsampled$data$default)  

sen_up <- 5325 / (5325 + 691)
spe_up <- 5148 / (5148 + 836)

#--------------------------------------
# 2. Titanic Exercise
#--------------------------------------
# Do it on your own

# titanic data set:

# PassengerId:  Unique ID of the passenger
# Survived:     Survived (1) or died (0)
# Pclass:       Passenger's class (1st, 2nd, or 3rd)
# Name:         Passenger's name
# Sex:          Passenger's sex
# Age:          Passenger's age
# SibSp:        Number of siblings/spouses aboard the Titanic
# Parch:        Number of parents/children aboard the Titanic
# Ticket:       Ticket number
# Fare:         Fare paid for ticket
# Cabin:        Cabin number
# Embarked:     Where the passenger got on the ship (C - Cherbourg, S - Southampton, Q = Queenstown)
getwd()
#setwd("/Users/Angelo/Desktop/MGSC310")
titanic <- read.csv("/Users/Angelo/Desktop/MGSC310/titanic.csv")
# Explore the data set
library(ggplot2)
ggplot(titanic, aes(x = Age)) + geom_histogram(aes(fill=factor(Survived)))

ggplot(titanic, aes(x = Age)) +
  geom_histogram() +
  facet_wrap(~ factor(Pclass) + factor(Survived)) + 
  theme_minimal()

titanic <- titanic[!is.na(titanic$Age),]

library(gmodels)
CrossTable(predicted_classes, true_outcome,
           prop.r = FALSE, prop.c = FALSE,prop.t = FALSE,
           prop.chisq = FALSE)

# Split data set into training (80%) and test (20%)
set.seed(310)
train_idx <- sample(1:nrow(titanic),
                    size=0.8*nrow(titanic),
                    replace=FALSE)
titanic_train <- titanic[train_idx,]
titanic-test <- titanic[-train_idx,]
# Estimate a logistic model:   survival ~ factor(Pclass) + sex + age
logit_model <- glm(Survived ~ factor(Pclass) + Age + Sex,
                   data = titanic_train,
                   family = binomial)
# Interpret the coefficients
summary(logit_model)
exp(logit_model$coefficients)
# Score the model

# Use two different threshold to predict classes

# Compute the Confusion Matrix

# Calculate Accuracy, Sensitivity, and Specificity

# Estimate a different model (you can arbitrarily choose predictors)

# Compare two models using ROC and AUC

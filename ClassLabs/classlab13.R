rm(list = ls()) #removing all variables

#######################################
#                                     #
#          Classification 3           #
#                                     #
#              Class 13               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - Estimating logit model using glm()
#   - false positives, false negatives
#   - Confusion matrices
#   - Calculating confusion matrices in R
#   - Assigning predicted classes given model probabilities
#   - How to select probability threshold - guideliens
#   - ROC curves
#   - Area Under ROC (AUC)
#   - Lift chart
#   - Sever class imbalance remedies
#   - In-class exercise: Titanic
#######################################


#--------------------------------------
# 1. Default Data
#--------------------------------------
# load data "Default" from ISLR package
library(ISLR)
data(Default)

Default$default_binary <- ifelse(Default$default=="Yes",1 ,0)

#--------------------------------------
# 2. Logistic Model
#--------------------------------------

# model 1: logistic model default ~ balance 
logit_fit1 <- glm(default ~ balance, 
                  family = binomial,
                  data = Default)

# model 2: logistic model default ~ student
logit_fit2 <- glm(default ~ student,
                  family = binomial,
                  data = Default)

# model 3: logistic model default ~ balance + income + student
logit_fit3 <- glm(default ~ balance + income + student,
                  family = binomial,
                  data = Default)


#--------------------------------------
# 3. Prediction Probabilities
#--------------------------------------

# storing scores from three different models
preds_DF <- data.frame(scores_mod1 = predict(logit_fit1, type = 'response'),
                       scores_mod2 = predict(logit_fit2, type = 'response'),
                       scores_mod3 = predict(logit_fit3, type = 'response'),
                       Default
)

head(preds_DF)

#--------------------------------------
# 4. ROC Curves
#--------------------------------------

# ROC plots
# use package plotROC
# install.packages("plotROC")
library(plotROC)


# comparing ROC for models 2 and 3
ggplot(preds_DF, aes(m = scores_mod2, d = default_binary)) +
  geom_roc(cutoffs.at = c(0.99,0.90,0.70,0.3,0.1,0.01))

ggplot(preds_DF, aes(m = scores_mod3, d = default_binary)) +
  geom_roc(cutoffs.at = c(0.99,0.90,0.70,0.3,0.1,0.01))

# plot ROC
mod2_roc <- ggplot(preds_DF, aes(m = scores_mod2, d = default_binary)) +
  geom_roc(cutoffs.at = c(0.99,0.90,0.70,0.3,0.1,0.01))

mod3_roc <- ggplot(preds_DF, aes(m = scores_mod3, d = default_binary)) +
  geom_roc(cutoffs.at = c(0.99,0.90,0.70,0.3,0.1,0.01))

mod2_roc

mod3_roc


# calcuate AUC (area under curve) for both models
calc_auc(mod2_roc)

calc_auc(mod3_roc)

# AUC: higher value means better model 
# 0.5 < AUC < 1
#--------------------------------------
# 5. Lift Chart
#--------------------------------------
library(caret)

# lift chart for model 1
creditlift <- lift(factor(default, levels = c("Yes", "No"))
                          ~ scores_mod1, data = preds_DF)

xyplot(creditlift, main = "X = Balance")
# lift chart for model 2
creditlift2 <- lift(factor(default, levels = c("Yes", "No"))
                   ~ scores_mod2, data = preds_DF)

xyplot(creditlift2, main = "X = Balance")


#--------------------------------------
# 6. Down-sampling and Up-sampling
#--------------------------------------
library('ROSE')

# down-sampling and up-sampling

# logit downsampled model


# logit up-sampled


# vanilla logit


# generate scores and class predictions

#--------------------------------------
# 7. Titanic Exercise
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


# Estimate a logistic model:   survival ~ factor(Pclass) + sex + age

# Score the model

# Use two different threshold to predict classes

# Compute the Confusion Matrix

# Estimate a different model (you can arbitrarily choose predictors)

# Compare two models using ROC and AUC


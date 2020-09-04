#######################################
#                                     #
#          Classification 2           #
#                                     #
#              Class 12               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - Estimating logit model using glm()
#   - Odds ratio interpretation
#   - Exponentiating logit coefficients to understand impact on odds ratio
#   - Generating individual predictions from estimated logit model
#   - "Scoring" dataset 
#   - Multiple logistic regression
#   - false positives, false negatives
#   - Confusion matrices
#   - Calculating confusion matrices in R
#   - Assigning predicted classes given model probabilities
#   - How to select probability threshold - guideliens
#######################################


#--------------------------------------
# 1. Default Data
#--------------------------------------
# load data "Default" from ISLR package
library(ISLR)
data(Default)
# we can force R to avoid scientif number formats
options(scipen = 999)

# create a binary (dummy) variable for default (outcome)
Default$default_binary <- ifelse(Default$default == "Yes", 1, 0)
head(Default)

#--------------------------------------
# 2. Logistic Model
#--------------------------------------

# model 1: logistic model default ~ balance 
logit_fit_1 <- glm(default ~ balance,
                   family = binomial,
                   data = Default)

summary(logit_fit_1)
# interpretation
exp(logit_fit_1$coefficients)

# model 2: including factor variable

logit_fit2 <- glm(default ~ student,
                  family = binomial,
                  data = Default)
# logistic model with student as independent var.
summary(logit_fit2)

exp(logit_fit2$coefficients)
# students are (1.499 - 1) 49% more likely to default comparted to non-students

# display exponentiated coefficients (optional)
# install.packages("jtools")
library("jtools")

summ(logit_fit2, exp = TRUE)
# model 3: multiple logistic regression

# logistic model default ~ balance + income + student
logit_fit3 <- glm(default ~ income + student,
                  family = binomial, 
                  Default)

summ(logit_fit3, exp = TRUE)
#--------------------------------------
# 3. Prediction Probabilities
#--------------------------------------

preds_df <- data.frame(scores_mod1 = predict(logit_fit_1, type = "response"),
                        scores_mod2 = predict(logit_fit2, type = "response"),
                       scores_mod3 = predict(logit_fit3, type = "response"), 
                       Default)

head(preds_df)
# storing scores from three different models

#--------------------------------------
# 4. Confusion Matrix
#--------------------------------------

# create the confusion matrix for different cutoffs
# first create a dataframe for predicted classes
preds_df <- data.frame(class_preds05 = ifelse(preds_df$scores_mod1 > 0.5, 1, 0),
                       class_preds06 = ifelse(preds_df$scores_mod1 > 0.6, 1, 0),
                       class_preds04 = ifelse(preds_df$scores_mod1 > 0.4, 1, 0),
                       preds_df)
                       
head(preds_df)        
# use table for confusion matrix
table(preds_df$class_preds05 , preds_df$default)                      
                       
# calculate sensitivity and specificity

# sensitivity: TP / P
sen <- 100 / (100+233)  

# specificity: TN / N
spe <- 9625 / (9625 + 42)





           
                       
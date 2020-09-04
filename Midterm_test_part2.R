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
library("plotROC")

covid_train <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/patients_train_A.csv")
covid_test <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/patients_test_A.csv")

#Question 2
# Create histograms of Age for patients who died and survived. 
# Points will be given for creativity and visual aesthetics. 
# Do you see any patterns?

covid_deathnum <- covid_train$Death==1
covid_death <- covid_train[covid_deathnum, ]

covid_survivenum <- covid_train$Death==0
covid_survive <- covid_train[covid_survivenum, ]

ggplot(covid_death, aes(covid_death$Age)) + geom_histogram(aes(fill = Sex)) + 
  labs(x = "Age",
       y = "Number of Patients who died",
       title = "Ages throughout Patients")

ggplot(covid_survive, aes(covid_survive$Age)) + geom_histogram(aes(fill = Sex)) + 
  labs(x = "Age",
       y = "Number of Patients who survived",
       title = "Ages throughout Patients")

#Question 3
# Print the correlation matrix for all numeric variables. 
# (use="complete.obs" option in cor() function may be used.) 
# Pick one variable and explain its correlation with Death and 
# explain why this correlation makes sense.

cormat <- cor(covid_train %>% select_if(is.numeric) %>% drop_na())
print(cormat[,"Death"])
corrplot(cormat)

#Question 4
# Estimate a logistic regression model to 
# predict Death as a function of age, sex, and chronic disease. 
# Be sure to copy the summary of your model to your output word file.  
logit_fit <- glm(Death ~ Age + Sex + ChronicDisease,
                 family = binomial,
                 data = covid_train)
summary(logit_fit)


#Question 5
# In a sentence, interpret the effect of "ChronicDisease" in the model above.

#Question 6
# Generate predicted scores from this model and use them to produce 
# ROC plots for the test and training sets data.

preds_train <- data.frame(scores = predict(logit_fit, type = "response"), covid_train)
preds_train <- preds_train %>% mutate(class_pred05 = ifelse(scores>0.5,1,0))

preds_test <- data.frame(scores = predict(logit_fit, newdata = covid_test, type = "response"), covid_test)
preds_test <- preds_test %>% mutate(class_pred05 = ifelse(scores>0.5,1,0))

train_ROC <- ggplot(preds_train, aes(m = scores, d = Death)) +
  geom_roc()
train_ROC

test_ROC <- ggplot(preds_test, aes(m = scores, d = Death)) +
  geom_roc()
test_ROC


#Question 7
# How good is this model? How do the ROC plots suggest we 
# should alter our cutoff probabilities when assigning predicted classes?
calc_auc(train_ROC)
calc_auc(test_ROC)


#Question 8
# Use a reasonable cutoff probability (justified by ROC plot), 
# generate class predictions. 
# Copy the confusion matrices in the output file and calculate sensitivity 
# and specificity for the model.

preds_trainm <- data.frame(scores = predict(logit_fit, type = "response"), covid_train)
preds_trainm <- data.frame(class_preds05 = ifelse(preds_train$scores > 0.5, 1, 0), preds_trainm)

preds_testm <- data.frame(scores = predict(logit_fit, newdata = covid_test, type = "response"), covid_test)
preds_testm <- data.frame(class_preds05 = ifelse(preds_test$scores > 0.5, 1, 0), preds_testm)

#Train confusion matrix
table(preds_trainm$Death,preds_trainm$class_preds05)

#Test confusion matrix
table(preds_testm$Death, preds_testm$class_preds05)

#Question 9
# Suppose you are an expert in health data science. 
# What is your main takeaway from the models above? 
# What are your suggestions to improve the prediction power?








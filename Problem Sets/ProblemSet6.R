
rm(list = ls()) #removing all variables


#Question a)

library(MASS)
data(Boston)

# a binary outcome for pricey home
Boston$PriceyHome <- ifelse(Boston$medv > 35, 1, 0)
# converting chas into a factor

Boston$chas <- factor(Boston$chas)
set.seed(2020)
trainSize <- 0.75
train_idx <- sample(1:nrow(Boston), size = nrow(Boston) * trainSize, replace=FALSE)
housing_train <- Boston[train_idx,]
housing_test <- Boston[-train_idx,]
head(housing_train)
 

#Question b)
 
library("doBy")
summaryBy(. ~ PriceyHome, housing_train, FUN=mean)
 

#Question c)
 
library(ggplot2)

ggplot(housing_train, aes(crim, lstat, color = factor(PriceyHome))) +
  geom_point() +
  facet_wrap('~PriceyHome') +
  labs(title = "Plot 1")

ggplot(housing_train, aes(tax, medv, color = factor(PriceyHome))) +
  geom_point() +
  facet_wrap('~PriceyHome') +
  labs(title = "Plot 2")

ggplot(housing_train, aes(black, medv, color = PriceyHome)) +
  geom_point() +
  labs(title = "Plot 3")
 

#Question d)
 
logit_fit <- glm(PriceyHome ~ chas,
                 data = housing_train,
                 family = binomial)
exp(logit_fit$coefficients[2])
 

#Question e)
 
logit_fit2 <- glm(PriceyHome ~ chas + crim + lstat + ptratio + zn + rm + tax + rad + nox,
                 data = housing_train,
                 family = binomial)
summary(logit_fit2)
#Need to exponentiate to interpret
exp(logit_fit2$coefficients)
 

#Question f)
 
preds_train <- data.frame(scores = predict(logit_fit2, type = "response"), housing_train)
preds_train <- data.frame(class_preds05 = ifelse(preds_train$scores > 0.5, 1, 0), preds_train)

preds_test <- data.frame(scores = predict(logit_fit2, newdata = housing_test, type = "response"), housing_test)
preds_test <- data.frame(class_preds05 = ifelse(preds_test$scores > 0.5, 1, 0), preds_test)
 

#Question g)
 
#Train confusion matrix
table(preds_train$class_preds05,preds_train$PriceyHome)
 
# Train Accuracy: 365/379 = 0.963
# Train True Positive: 26
# Train True Negative: 339
# Sensitivity: 26/37 = 0.703
# Specificity: 339/342 = 0.991
# False positive rate: 3/342 = 0.0089
 
#Test confusion matrix
table(preds_test$PriceyHome, preds_test$class_preds05)
 
# Test Accuracy: 120/127 = 0.945
# Train True Positive: 7
# Train True Negative: 113
# Sensitivity: 7/10 = 0.70
# Specificity: 113/117 = 0.966
# False positive rate: 4/117 = 0.034

#Question h)
#Typically, we would like to have a specificity and sensitivity close to 1, however in this case it is not extremely important to have a high sensitivity due to the fact the stakes are not high, like if we were trying to predict bomb locations in the military. However, if you are a real estate agent and are trying to create an extremely accurate model to help determine price on a house, you would want to raise the sensitivity so customers would see how close you are to the market. Also it would give you the highest amout of profit. The cutoff is at 0.5 right now so in order to raise sensitivity, I would lower the cutoff to about 0.45 to help raise the sensitivity rating by allowing for more positive predictions. 

#Question i)
 
library(plotROC)
train_ROC <- ggplot(preds_train, aes(m = scores, d = PriceyHome)) + 
geom_roc(labelsize = 3.5, cutoffs.at = c(.99,.9,.8,.5,.3,.1,.01)) + labs(x = "True Positive Fraction", 
                                                        y = "False Positive Fraction",
                                                        title = "ROC Curve, Train")
train_ROC
 

 
test_ROC <- ggplot(preds_test, aes(m = scores, d = PriceyHome)) + 
geom_roc(labelsize = 3.5, cutoffs.at = c(.99,.9,.8,.5,.3,.1,.01)) + labs(x = "True Positive Fraction", 
                                                        y = "False Positive Fraction",
                                                        title = "ROC Curve, Test")
test_ROC
 

#Question j)
 
calc_auc(train_ROC)
calc_auc(test_ROC)
 




rm(list = ls()) #removing all variables


#######################################
#                                     #
#           Model Selection           #
#                                     #
#              Class 19               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - Ridge and Lasso for classification
#   - Prediction using Ridge and Lasso
#   - Model Comparison
#   - Yelp Challenge (large feature size)
#######################################

# for cv.glmnet
library("glmnet")
library("glmnetUtils")
# for Caravan data
library("ISLR")
# for geom_roc (plotting roc) and calc_auc
library("ggplot2")
library("plotROC")
# for RMSE
library("caret")

#------------------------------------------------
### Lasso: Classification
#------------------------------------------------
# load the Caravan dataset from ISLR, outcome: Purchase
data(Caravan)
head(Caravan$Purchase)
table(Caravan$Purchase)
dim(Caravan)
# split the dataset into training and test: 4000 for training and the remaining for test
set.seed(310)
ind <- sample(1:nrow(Caravan), size = 4000, replace=FALSE) 
Caravan_train <- Caravan[ind, ]
Caravan_test <- Caravan[-ind, ]

# run a logistic regression as the base model
# Purchase against all the variables
caravan_logit <- glm(Purchase ~ .,
                     data = Caravan_train,
                     family = "binomial")



# score the model for test and training
scores_train <- predict(caravan_logit,
                        newdata = Caravan_train,
                        type = "response")
preds_train_df <- data.frame( scores = scores_train, 
                              true = Caravan_train$Purchase)

#for test
scores_test <- predict(caravan_logit,
                        newdata = Caravan_test,
                        type = "response")
preds_test_df <- data.frame( scores = scores_test, 
                              true = Caravan_test$Purchase)

# create ROC plots for test and training and calculate the AUC
roc_train <- ggplot(preds_train_df, aes(m=scores, d=true)) +
  geom_roc(cutoffs.at = c(0.9,0.7,0.5,0.3,0.1,0.05))
roc_train
calc_auc(roc_train) # 0.79

roc_test <- ggplot(preds_test_df, aes(m=scores, d=true)) +
  geom_roc(cutoffs.at = c(0.9,0.7,0.5,0.3,0.1,0.05))
roc_test
calc_auc(roc_test) # 0.73


#--------------------------------------
# now, use lasso model to train a model to classify Purchase
# by default, cv.glmnet uses kfold = 10 for cross-validation.
#   change it to 5 by: nfolds = 5

caravan_lasso <- cv.glmnet(Purchase ~ .,
                           data = Caravan_train,
                           alpha = 1,
                           family = "binomial",
                           type.measure = "auc",
                           nfolds = 5)

# plot the lasso AUC against different values for lambda
plot(caravan_lasso)

# what the best lambda?
lambda_min <- caravan_lasso$lambda.min
caravan_lasso$lambda.1se

# predict the scores (probabilities)
scores_lasso_train <- predict(caravan_lasso,
                              newdata = Caravan_train,
                              type = "response",
                              s = lambda_min)

scores_lasso_test <- predict(caravan_lasso,
                              newdata = Caravan_test,
                              type = "response",
                              s = lambda_min)

preds_lasso_train <- data.frame(scores = scores_lasso_train[,1],
                                true = Caravan_train$Purchase)
preds_lasso_test <- data.frame(scores = scores_lasso_test[,1],
                                true = Caravan_test$Purchase)
# Plot the ROC
roc_lasso_train <- ggplot(preds_lasso_train, aes(m=scores, d=true)) +
  geom_roc(cutoffs.at = c(0.9,0.7,0.5,0.3,0.1,0.05))
roc_lasso_train

roc_lasso_test <- ggplot(preds_lasso_test, aes(m=scores, d=true)) +
  geom_roc(cutoffs.at = c(0.9,0.7,0.5,0.3,0.1,0.05))
roc_lasso_test
# Calculate AUC for test and compare it with logit
calc_auc(roc_lasso_train)
calc_auc(roc_lasso_test)

#       logistic    Lasso
# train   0.79      0.77
# test    0.73      0.76

#------------------------------------------------
### Yelp Challenge
#------------------------------------------------

# 1. read the data set "yelp.csv"
yelp <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/yelp.csv")
# 2. check the dimensions
dim(yelp)
# 3. split the data into training and test
#   use: set.seed(310)
set.seed(310)
ind <- sample(1:nrow(yelp), size = 4500, replace=FALSE) 
yelp_train <- yelp[ind, ]
yelp_test <- yelp[-ind, ]

# 4. the outcome variable is upvotes. check the histogram and summary
ggplot(yelp, aes(upvotes)) +
  geom_histogram()
# 5. build a linear regression model as the base model
#     (use all variables)
mod <- lm(upvotes ~ .,
          data = yelp_train)
summary(mod)

# 6. predict the outcome for train and test and measure RMSE
preds_train1 <- predict(mod, newdata = yelp_train)
preds_test1 <- predict(mod, newdata = yelp_test)

preds_train1_df <- data.frame(true = yelp_train$upvotes,
                              pred = predict(mod, newdata = yelp_train),
                              resid = yelp_train$upvotes - predict(mod, newdata =  yelp_train))

preds_test1_df <- data.frame(true = yelp_test$upvotes,
                             pred = predict(mod, newdata = yelp_test),
                             resid = yelp_test$upvotes - predict(mod, newdata = yelp_test))

RMSE(preds_train1_df$pred, preds_train1_df$true)
RMSE(preds_test1_df$pred, preds_test1_df$true)

# 7. build a lasso model (use nfolds = 5)
# note that it may take a while to run


# 8. what the best lambda?



# 9. print the non-zero coefficients for lambda min
# alternatively you can use coefplot package
library(coefplot)
# use extract.coef()

# 10. predict upvotes for training and test and measure RMSE



# 11. which model has a lower RMSE on training? what about test? How do you explain?



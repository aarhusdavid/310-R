rm(list = ls()) #removing all variables

#######################################
#                                     #
#    Resampling - Cross-Validation    #
#                                     #
#              Class 15               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - for loops in R
#   - LOOCV
#   - K-fold hold-out
#######################################

#--------------------------------------
# 1. for loops
#--------------------------------------

# for loops in R
# syntax:
# for( val in sequence){
#     statement
#   }
# print 1 to 10 using for
for (i in 1:10){
  print(i)
}

# print powers of 2 from 1 to 15
for (i in 1:15){
  print(2^i)
}


#--------------------------------------
# 2. load data
#--------------------------------------
# use Auto data set from ISLR package
library(ISLR)
data(Auto)

# removing the column: name (everything else would be numeric)
Auto_sub <- Auto[,-9]

head(Auto_sub)
#--------------------------------------
# 3. LOOCV
#--------------------------------------
# for loop of model
preds_LOOCV <- NULL
# alternatively:
# preds_LOOCV <- rep(NA,nrow(Auto_sub))

# create an empty list to store models (optional)
# mod_list <- list()

for (i in 1:nrow(Auto_sub)){
  # train the model
  mod <- lm(mpg ~ ., data=Auto_sub[-i,]) # exclusion of ith row
  # predict the model for the left out obs.
  pred <- predict(mod, newdata=Auto_sub[i,]) # prediction for the ith row
  # store the prediction
  preds_LOOCV[i] <- pred
  # optional: store the models
  # mod_list[[i]] <- mod
}

# note that . can be used to represent all the variables in the regression model

# train the insample model to compare the errors
mod_insample <- lm(mpg ~ ., data = Auto_sub)
preds_insample <- predict(mod_insample)

# store the hold-out predictions and in_sample predictions
preds_DF <- data.frame(
  preds_LOOCV = preds_LOOCV,
  preds_insample = preds_insample,
  true = Auto_sub$mpg
)

# compute RMSE LOOCV 
# use caret package
library(caret)

# RMSE for LOOCV
RMSE(preds_DF$preds_LOOCV, preds_DF$true)
# RMSE for insample
RMSE(preds_DF$preds_insample, preds_DF$true)

# in-sample RMSE < LOOCV RMSE

#--------------------------------------
# 4. k-fold Cross validation
#--------------------------------------

# creating folds (using createFolds from caret package)
Auto_sub$folds <- createFolds(Auto_sub$mpg,
                              k = 10,
                              list = FALSE)

Auto_sub$folds

head(Auto_sub)

### K-Fold Cross Validation
nfolds <- 10
# empty dataframe
preds_10foldCV <- data.frame(
  folds = Auto_sub$folds,
  preds = rep(NA,nrow(Auto_sub))
)

# for loop for K-Fold CV
for (i in 1:nfolds){
  # train the model using all the obs except the ith group
  mod <- lm(mpg ~ . -folds,
            data = Auto_sub[Auto_sub$folds != i, ])
  # prediction for the ith group
  pred <- predict(mod, newdata=Auto_sub[Auto_sub$folds == i, ])
  # store the predictions
  preds_10foldCV[preds_10foldCV$folds==i, "preds"] <- pred
}

head(preds_10foldCV$preds)

# compare LOOCV, K-fold CV and insample errors
RMSE(preds_10foldCV$preds, preds_DF$true)

# In-sample RMSE < LOOCV < K-fold
#######################################
#                                     #
#       Bagging - Random Forest       #
#                                     #
#              Class 21               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#  - Bagging algorithm
#  - Estimating random forest models
#  - mtry and ntree parameters
#  - Variable "importance" 
#  - explain_forest() to explain random forests
#  - Variable depth plots
#  - depth versus importance
#  - Interaction plots
#  - Variable importance plots
#  if time permits:
#  - Tuning parameters of mtry and ntree in random forests
#  - Cross validating to select these tuning parameters
#######################################

#------------------------------------------------
### Bagging
#------------------------------------------------
# using Boston data
library(MASS)
data(Boston)

install.packages("randomForest")
library(randomForest)
# split data set into 65% training and 35% test
set.seed(310)
train_indx <- sample(1:nrow(Boston), size = 0.65*nrow(Boston), replace = FALSE)
Boston_train <- Boston[train_indx,]
Boston_test <- Boston[-train_indx,]

# using random forest function with mtry = p
bag.boston <- randomForest(medv ~ . ,
                           data = Boston_train,
                           ntry = 13,
                           ntree = 500,
                           importance = TRUE)


# print the model
bag.boston

# performance of the model
# predict median value of housing for test
preds_bag <- predict(bag.boston, newdata = Boston_test)
# plot true vs predicted
plot(preds_bag, Boston_test$medv)
abline(0,1,col="red")

# RMSE

# or RMSE from caret
library("caret")

RMSE(preds_bag, Boston_test$medv)
# 3.25

#------------------------------------------------
### Random Forest
#------------------------------------------------
# random forest with mtry = 6 (use 500 trees)
rf.boston <- randomForest(medv ~ . ,
                          data = Boston_train,
                          ntry = 6,
                          ntree = 500,
                          importance = TRUE)


# print the model
rf.boston
# performance of the model on test
preds_rf <- predict(rf.boston, newdata = Boston_test)

RMSE(preds_rf, Boston_test$medv)
# 3.27

# Exercise: Build a tree Boston train and compare its
#   performance on test with random forest

#------------------------------------------------
### Variable Importance
#------------------------------------------------
# importance
importance(rf.boston)

# importance plot
varImpPlot(rf.boston)

# Note: We will explore randomForestExplainer later
#       That gives more information and insights

#------------------------------------------------
### Classification
#------------------------------------------------
# for classification use type=classification in 
#   randomForest()

# Execise: Apply a bagging and random forest classifier 
#   on titanic dataset on your own

#------------------------------------------------
### Random Forest Explanations
#------------------------------------------------
# importance
importance(rf.boston)

varImpPlot(rf.boston)

install.packages("randomForestExplainer")
library(randomForestExplainer)
library(ggplot2)

# plot min depth distribution
plot_min_depth_distribution(rf.boston)
# plot variable two-way importance measure
plot_multi_way_importance(rf.boston)
# plot variable two-way importance measure
# change x to "mse_increase" and y to "node_purity_increase"
plot_multi_way_importance(rf.boston, x_measure = "mse_increase",
                          y_measure = "node_purity_increase")
# plot two variables interaction effect: lstat and rm
plot_predict_interaction(rf.boston, Boston_train, "lstat", "rm")

# explanation file 
explain_forest(rf.boston, interactions=TRUE, data=Boston_train)

#------------------------------------------------
### Random Forests Tuning
#------------------------------------------------
# optional

#rf_mods <- list()
#test_err <- NULL

oob_err <- NULL
for(mtry in 1:9){
  rf_fit <- randomForest(medv ~ ., 
                         data = Boston_train,
                         mtry = mtry,
                         ntree = 500)
  oob_err[mtry] <- mean(rf_fit$mse)
  
  cat(mtry," ")
}

results_DF <- data.frame(mtry = 1:9,
                         oob_err)
ggplot(results_DF, aes(x = mtry, y = oob_err)) + geom_point()


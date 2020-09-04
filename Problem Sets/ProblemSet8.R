# Name: David Aarhus
# Problem Set 8

rm(list = ls()) #removing all variables

# load Auto dataset
library(ISLR)
data(Auto)

#==Question a and b
set.seed(310) #seed set
indx <- sample(1:nrow(Auto), size = 0.70*nrow(Auto),replace = FALSE)
auto_train <- Auto[indx,] #creates train set
auto_test <- Auto[-indx,] #creates test set

#==Question c
library("tree")

#regression tree on mpg, using all variables except vehicle name
tree <- tree(mpg ~ cylinders + displacement + horsepower + weight +
                    acceleration + year + origin,
                  data = auto_train)

#==Question d
# plot the tree and add text to objects
plot(tree) #plots tree
text(tree) #labels tree

#==Question e
#Describe tree
# There are alot of cars with less than 5.5 cylinders
# Also there seems to be only 1 car that has horse power greater than 127.5 and is over 5.5 cylinders

#==Question f
preds_test <- predict(tree, newdata = auto_test) #test predictions
preds_train <- predict(tree, newdata = auto_train) #train predictions

# calculate RMSE
library("caret")
RMSE(preds_test, auto_test$mpg) # -> 3.987852
RMSE(preds_train, auto_train$mpg) # -> 2.87646

#==Question g
cv.tree <- cv.tree(tree)
cv.tree

best_tree_indx <- which.min(cv.tree$dev) #finds best tree size
cv.tree$size[best_tree_indx]
# -> 7

#==Question 1h
pruned_tree_auto <- prune.tree(tree, best = 7) #prunes tree 

plot(pruned_tree_auto) #plots pruned tree
text(pruned_tree_auto) #labels pruned tree

preds_test_pruned <- predict(pruned_tree_auto, newdata = auto_test) #predictions from train tree
preds_train_pruned <- predict(pruned_tree_auto, newdata = auto_train) #predictions from test tree

RMSE(preds_test_pruned, auto_test$mpg) # 3.987852
RMSE(preds_train_pruned, auto_train$mpg) # 2.87646

#==Question 1i
# estimate a random forest
library("randomForest")

bag <- randomForest(mpg ~ cylinders + displacement + horsepower + weight +
                      acceleration + year + origin,,
                         data = auto_train,
                         mtry = 4,
                         ntree = 500, 
                         importance = TRUE)

# The mtry parameter is the number of variables available for splitting each node. 
# This comes into factor when you may have limits on memory and
# computational limitations. 

#==Question 1j
preds_bag_test <- predict(bag, newdata = auto_test) #test predictions
preds_bag_train <- predict(bag, newdata = auto_train) #train predictions

# RMSE
RMSE(preds_bag_test, auto_test$mpg) # -> 3.041922
RMSE(preds_bag_train, auto_train$mpg) # -> 1.269536

#==Question 1k
importance(bag)
varImpPlot(bag)

#==Question 1l
# The tree and pruned tree models appeared to perform the same with similar RMSE scores
# However, the randomForest model came out on top with the lowest RMSE score for both
# the training set and test set. I think this is because since a randomForest is made
# up of several trees, this allows the model to be more precise and accurate which is
# shown in the RMSE scores. 




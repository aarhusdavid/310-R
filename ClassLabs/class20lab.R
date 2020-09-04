rm(list = ls()) #removing all variables


#######################################
#                                     #
#           Decision Trees            #
#                                     #
#              Class 20               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
# 1. Understanding from partitioning data to regression tree representation
# 2. Nodes, leafs, and split terminology
# 3. How a regression tree is constructed
# 4. How leaf values are constructed. 
# 5. How we predict using trees. 
# 6. Pruning trees
# 7. Estimating Regression trees in R
# 8. Cross validating tree complexity
# 9. Vizualizing regression trees
#######################################

#------------------------------------------------
### Regression Tree
#------------------------------------------------
#install.packages("tree")
library("tree")
# Load Hitters data from ISLR
library("ISLR")
data(Hitters)

# Do necessary data transformation:
#   - dropping missing
#   - log transformation of Salary
Hitters <- Hitters[!is.na(Hitters$Salary), ]

library("ggplot2")
ggplot(Hitters, aes(Salary)) +
  geom_histogram() +
  theme_minimal()

Hitters$logsalary <- log(Hitters$Salary)

ggplot(Hitters, aes(logsalary)) +
  geom_histogram() +
  theme_minimal()

# Split data into training (75%) and test (25%)
set.seed(310)
train_indx <- sample(1:nrow(Hitters), 0.75 * nrow(Hitters), replace=FALSE)
Hitters_train <- Hitters[train_indx, ]
Hitters_test <- Hitters[-train_indx, ]


# Train the tree
# outcome: logSalary
# predictors: AtBat + Hits + HmRun + RBI + Walks + Years + Division + Assists + Errors
tree_hitters <- tree(logsalary ~ AtBat+Hits+HmRun+RBI+Walks+Years+Division+Assists, Hitters_train)


# Report the results
tree_hitters
summary(tree_hitters) #RMSE for train = 0.215

# Plot the tree
plot(tree_hitters)
text(tree_hitters)

#make it pretty
plot(tree_hitters)
text(tree_hitters, pretty = 0, digits = 3)

# Predict the test
preds_test <- predict(tree_hitters, newdata = Hitters_test)
head(preds_test)
#exp(preds_test) #converts back to dollars

# Measure RMSE test
library("caret")
RMSE(preds_test, Hitters_test$logsalary) #0.65

# Plot predicted vs true



# cross-validation to find best tree size
cv.tree_hitters <- cv.tree(tree_hitters)

# report the results
cv.tree_hitters

# find the best tree size
best_tree_index <- which.min(cv.tree_hitters$dev)
cv.tree_hitters$size[best_tree_index]

# plot the error vs tree 
plot(cv.tree_hitters$size, cv.tree_hitters$dev, type = "b")


# prune the tree by the best size
pruned_tree_hitters <- prune.tree(tree_hitters, best = 6)

# plot the pruned tree
plot(pruned_tree_hitters)
text(pruned_tree_hitters)


# Predict the test
preds_test_pruned <- predict(pruned_tree_hitters, newdata = Hitters_test)
RMSE(preds_test_pruned, Hitters_test$logsalary) #0.58

# Plot predicted vs true






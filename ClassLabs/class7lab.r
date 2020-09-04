rm(list = ls()) #removing all variables

#######################################
#                                     #
#         Linear Regression           #
#                                     #
#              Class 7                #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - Data partition into test and train
#   - Run linear regression using lm()
#   - Interpret the results of lm
#
#   Data Exploration
#   - Check for missing values
#   - Identify and remove outliers
#   - Correlation between variables
#   - Correlation map using corrplot()
#   - scatterplots
#   - Using frequency table
#######################################

#######################################
### Load Boston dataframe from MASS package
library("MASS")# Now the data set is contained in the object Boston.
Boston <- data.frame(Boston) #the columns give information about the population at Boston


#######################################
### Data Exploration
library("corrplot")
corrplot(cor(Boston))

library("ggplot2")

ggplot(Boston, aes(crim, medv, )) +
  geom_point()

ggplot(Boston, aes(crim, medv, color = factor(chas) )) +
  geom_point(alpha = 0.5, size = 0.3) + #alpha changes transparency #size changes size of points
  theme_minimal() +
  labs(title = "Value of Housing over Crime Rate",
       x = "Crime Rate",
       y = "Median Value of Houses")

ggplot(Boston, aes(crim, medv)) +
  geom_point(aes(color = factor(chas)), alpha = 0.5, size = 0.3) + #alpha changes transparency #size changes size of points
  geom_smooth(method = 'lm') +
  theme_minimal() +
  labs(title = "Value of Housing over Crime Rate",
       x = "Crime Rate",
       y = "Median Value of Houses")
#######################################
### Data partitioning

# training (75%) - test (25%)
set.seed(310)
train_index <- sample(1:nrow(Boston), size = 0.75*nrow(Boston), replace = FALSE) 
Boston.train <- Boston[train_index, ]
Boston.test <- Boston[-train_index, ]
# check the dimensions
dim(Boston)
dim(Boston.train)
dim(Boston.test)
#######################################
### Linear Regression
my_model <- lm(medv ~ crim, Boston.train)
my_model

summary(my_model)

coef(my_model)

predict_train <- predict(my_model)
predict_test <- predict(my_model, Boston.test)
predict_test


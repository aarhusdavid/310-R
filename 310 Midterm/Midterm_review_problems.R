rm(list = ls()) #removing all variables

#1. Create a summary statistics table.
#getwd()
#setwd("/Users/DavidAarhus/Documents/310 R/Datasets")
donations <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/donation.csv")
summary(donations)

#2. Print the frequency table for the factor variable Homeowner. Compare variable
# averages for different values of Homeowner. Do you find any interesting patterns?
table(donations$Homeowner)

library("doBy")
summaryBy(. ~ Homeowner, donations, FUN = mean)


# 3. Partition the data into 70% training and 30% validation (test) using seed 310.

set.seed(310)
train_index <- sample(1:nrow(donations), size = 0.70*nrow(donations), replace = FALSE) 
donations.train <- donations[train_index, ]
donations.test <- donations[-train_index, ]

# 4. Using the training set, run a linear regression model with all the variables.
mod1 <- lm(Donation ~ ., donations.train)
summary(mod1)

# 5. What are the significant predictors? How do you interpret them?

# 6. Predict the outcome for the training and test sets.
preds_train1 <- predict(mod1)
preds_train1_df <- data.frame(true = donations.train$Donation,
                              pred = preds_train1,
                              resid = mod1$residuals)

preds_test1 <- predict(mod1, newdata = donations.test)
preds_test1_df <- data.frame(true =donations.test$Donation,
                             pred = preds_test1)

#7. Calculate the residuals for train and test. Are the errors heteroskedastic or
#homoscedastic?

ggplot(preds_train1_df, aes(pred, resid)) +
  geom_point()

#8. What is RMSE for test and train? Do you think there is overfitting problem?
library(caret)

# training error
RMSE(preds_train1_df$pred, preds_train1_df$true)
# test error
RMSE(preds_test1_df$pred, preds_test1_df$true)

#9. Run a best subset model. Which model is the best model?
library("leaps")

forward_mod <- regsubsets(Donation ~ .,
                          data = donations.train,
                          nvmax = 17,
                          method = "forward")

summary(forward_mod)

# train a forward stepwise model (use # of max variables = nvmax)
                         


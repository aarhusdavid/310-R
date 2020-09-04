
## Libraries
library("tidyverse")
library("ggplot2")
library("caret")
library("doBy")
library("glmnet")
library("glmnetUtils")
library("leaps")
library("plotROC")
library("coefplot")
  
rm(list = ls()) #removing all variables
  
# Loads in Data
suicide <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/suicide.csv")
names(suicide)
  
# Data Transformation
#remove meaning less columns (HDI.for.year)
suicide$HDI.for.year <- NULL
suicide$country.year <- NULL
#create new column (gdpM, gdpB, suicide_hvy)
suicide$gdpM <- suicide$gdp_for_year..../1e+6
suicide$gdpB <- suicide$gdp_for_year..../1e+9
suicide$pop100000 <- suicide$population*1e-5
suicide$suicide_rate_per100000capita <- suicide$pop100000/suicide$suicides_no

#identified whether a country was suicide heavy or not by first running summary statistics on the dataset to find the average suicide rate for each country. This then allowed me to identify the average suicide rate for the dataset. After finding this I used a suicide rate just a tad higher than the average to identify whether a country was suicide heavy (in relation to the worldwide average)
suicide$suicide_hvy <- ifelse(suicide$suicide_rate_per100000capita > 0.56, 1, 0)

NaRV.omit <- function(x){
  ## Author: Rene Locher
  ## Version: 2005-10-17
  
  if (is.vector(x)) {
    if (is.numeric(x)) x <- na.omit(x[is.finite(x)]) else
      x <- na.omit(x)
  } else if (is.factor(x)) {
    x <- na.omit(x)
  } else if (is.data.frame(x)) {
    x.num <- as.matrix(x[,sapply(x, is.numeric)])
    ri <- (!apply(x.num, MARGIN = 1,
                  function(x) sum(is.infinite(x))>0) &
             !apply(x, MARGIN = 1, function(x) sum(is.na(x))>0))
    x <- x[ri,,drop=FALSE]
    ## class omit is incompatible with class data.frame
    ## attributes(x) <- c(attributes(x),list(na.action=which(!ri),class="omit"))
  } else if (is.matrix(x)) {
    if(is.numeric(x)) {
      ri <- !apply(x, MARGIN = 1,
                   function(x) {sum(is.na(x)|is.infinite(x))>0})
      x <- x[ri,,drop=FALSE]
      attributes(x) <- c(attributes(x),list(na.action=which(!ri),class="omit"))
    } else x <- na.omit(x)
  } else {
    warning("'x' is neither a vector, nor a factor, nor a data.frame nor a matrix. \n'x' is returned unchanged\n")
    return(x)
  }
  return(x)
}## NaRV.omit

suicide <- NaRV.omit(suicide)

  


# Summary tables
 
summary(suicide)
 
summaryBy(suicides_no ~ country, suicide, FUN = mean)
 
summaryBy(gdpB ~ country, suicide, FUN = mean)
  
summaryBy(suicides_no ~ sex, suicide, FUN = mean)
  

# GGPLOTS
 
ggplot(suicide, aes(generation)) +
  geom_bar(aes(fill = sex)) +
  labs(x = " ", y = "Count", title = "Suicide Totals vs Generations")
  
ggplot(suicide, aes(suicides.100k.pop, gdpB)) +
  geom_point(aes(color = age)) +
  labs(x = "Suicides per 100k pop ", y = "GDP (Billions)", title = "GDP (Billions) vs Suicides per 100k pop")
 
ggplot(suicide, aes(year, suicides_no)) +
  geom_point(aes(color = sex)) +
  labs(x = "Year", y = "Suicide Count", title = "Suicides vs Year")

ggplot(suicide, aes(population, suicides.100k.pop )) +
  geom_point(aes(color = generation)) +
  labs(x = "Population", y = "Suicides per 100k pop", title = "Suicides per 100k pop Vs Population")
  
ggplot(suicide, aes(year, suicides.100k.pop)) +
  geom_point(aes(color = generation)) +
  labs(x = "Year", y = "Suicides per 100k pop", title = "Suicides per 100k pop Vs Year")
  

# Training Split
set.seed(310)
train_indx <- sample(1:nrow(suicide), 0.70 * nrow(suicide), replace=FALSE)
suicide_train <- suicide[train_indx, ]
suicide_test <- suicide[-train_indx, ]
  

 #Linear Regression
 
# We are using a linear model to see the affect of certain variables on the number of suicides in different countries
Linear_mod <- lm(suicide_rate_per100000capita ~ ., suicide_train)
summary(Linear_mod)
  
# This linear model helps point out the factors other than countries that play a key role in the amount of suicides
Linear_mod_noCountries <- lm(suicide_rate_per100000capita ~ year + sex 
                             + age + generation + country
                             + gdpB + gdpM, suicide_train)
summary(Linear_mod_noCountries)
  

# Linear Regression Predictions (Linear_mod_noCountries)
 
preds_train1 <- predict(Linear_mod_noCountries, newdata = suicide_train)
preds_test1 <- predict(Linear_mod_noCountries, newdata = suicide_test)

preds_train1_df <- data.frame(true = suicide_train$suicide_rate_per100000capita,
                              pred = predict(Linear_mod_noCountries, newdata = suicide_train),
                              resid = suicide_train$suicide_rate_per100000capita - predict(Linear_mod_noCountries, newdata =  suicide_train))

preds_test1_df <- data.frame(true = suicide_test$suicide_rate_per100000capita,
                             pred = predict(Linear_mod_noCountries, newdata = suicide_test),
                             resid = suicide_test$suicide_rate_per100000capita - predict(Linear_mod_noCountries, newdata = suicide_test))
  

# Linear Regression Predictions (Linear_mod)
 
preds_train2 <- predict(Linear_mod, newdata = suicide_train)
preds_test2 <- predict(Linear_mod, newdata = suicide_test)

preds_train2_df <- data.frame(true = suicide_train$suicide_rate_per100000capita,
                              pred = predict(Linear_mod, newdata = suicide_train),
                              resid = suicide_train$suicide_rate_per100000capita - predict(Linear_mod, newdata =  suicide_train))

preds_test2_df <- data.frame(true = suicide_test$suicide_rate_per100000capita,
                             pred = predict(Linear_mod, newdata = suicide_test),
                             resid = suicide_test$suicide_rate_per100000capita - predict(Linear_mod, newdata = suicide_test))
  

# Plot predictions (linear_mod_noCountries)
 
ggplot(preds_train1_df, aes(x=pred, y=resid)) +
  geom_point() + 
  geom_smooth(se=FALSE)
  
ggplot(preds_test1_df, aes(x=pred, y=resid)) +
  geom_point() + 
  geom_smooth(se=FALSE)
  

# Plot predictions (linear_mod)
 
ggplot(preds_train2_df, aes(x=pred, y=resid)) +
  geom_point() + 
  geom_smooth(se=FALSE)
  
ggplot(preds_test2_df, aes(x=pred, y=resid)) +
  geom_point() + 
  geom_smooth(se=FALSE)
  

# MSE for train and test (linear_mod_noCountries)
 
RMSE(preds_train1_df$pred, preds_train1_df$true)
  
RMSE(preds_test1_df$pred, preds_test1_df$true)
  

# MSE for train and test (linear_mod)
 
RMSE(preds_train2_df$pred, preds_train2_df$true)
  
RMSE(preds_test2_df$pred, preds_test2_df$true)
  

# Logistic Regression
 
logit_fit <- glm(suicide_hvy ~ year + sex + age + generation + country + gdp_per_capita...., family = binomial, suicide_train)
summary(logit_fit)
  

# Logistic Regression Predictions
 
preds_train <- data.frame(scores = predict(logit_fit, type = "response"), suicide_train)
preds_train <- data.frame(class_preds05 = ifelse(preds_train$scores > 0.5, 1, 0), preds_train)

preds_test <- data.frame(scores = predict(logit_fit, newdata = suicide_test, type = "response"), suicide_test)
preds_test <- data.frame(class_preds05 = ifelse(preds_test$scores > 0.5, 1, 0), preds_test)
  

# Train confusion matrix
table(preds_train$suicide_hvy,preds_train$class_preds05)
# Sensitivity = 0.9641
# Specificity = 0.8621

# Test confusion matrix
table(preds_test$suicide_hvy, preds_test$class_preds05)
# Sensitivity = 0.9618
# Specificity = 0.8646
   

# Train ROC Plot
train_ROC <- ggplot(preds_train, aes(m = scores, d = suicide_hvy)) +
  geom_roc(cutoffs.at = 0.56)
train_ROC
   

# Test ROC Plot
test_ROC <- ggplot(preds_test, aes(m = scores, d = suicide_hvy)) +
  geom_roc(cutoffs.at = 0.56)
test_ROC
   

# Train AUC score
calc_auc(train_ROC)
   

# Test AUC score
calc_auc(test_ROC)
   

# Lasso Regression
  
# estimate Lasso mod 
lasso_mod <- cv.glmnet(suicide_rate_per100000capita ~ .,
                       data = suicide_train,
                       alpha = 1)
   
# print the coefficients for lambda.min and lambda.1se
lasso_mod$lambda.min
lasso_mod$lambda.1se
   
# put in a matrix
coef(lasso_mod, s = lasso_mod$lambda.min)
coef(lasso_mod, s = lasso_mod$lambda.1se)
   
# plot lasso mse for different values of lambda
plot(lasso_mod)
   
# explore how coefficients 
# change as we change lambda
coefpath(lasso_mod)
   

  
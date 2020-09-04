rm(list = ls()) #removing all variables
#a) Navigate to the UCI Machine Learning website and download the data folder for the Bike Sharing Dataset. Read the file day.csv into R using read.csv and store it as the object Bike_DF. Read the description to get familiar with the variables.
 Bike_DF <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/day.csv")
  

#b) Do some basic data cleaning on Bike_DF to ensure factor variables are recorded as factors. Then run the command sapply(Bike_DF, is.factor) to ensure the columns in your data frame that should be factors have been converted to factors appropriately. (Hint: the predictors weather situation, season,year, month, holiday, weekday, and workingday should be factor.)
                                                                                                                                                                                                                                                             
library("tidyverse")
library("forcats")

Bike_DF$season <- as.factor(Bike_DF$season)
Bike_DF$workingday <- as.factor(Bike_DF$workingday)
Bike_DF$holiday <- as.factor(Bike_DF$holiday)
Bike_DF$weathersit <- as.factor(Bike_DF$weathersit)
Bike_DF$yr <- as.factor(Bike_DF$yr)
Bike_DF$mnth <- as.factor(Bike_DF$mnth)
Bike_DF$weekday <- as.factor(Bike_DF$weekday)

sapply(Bike_DF, is.factor)
  

#c) We now perform some feature transformation (creation of derived variables). 
 
Bike_DF <- Bike_DF %>% select(-instant)
Bike_DF <- Bike_DF %>% select(-dteday)

  
#Create 4 new variables as squared terms of temperature, feeling temperature, humidity, and windspeed. Refer to data description to find the corresponding predictors in the data.
 
Bike_DF <- Bike_DF %>% mutate(temp_sq = temp * temp,
                              atemp_sq = atemp * atemp,
                              hum_sq = hum * hum,
                              windspeed_sq = windspeed * windspeed)
  

#d) Split our data into test and training splits of size 30%/70% each. Use 310 as the seed number.
 
set.seed(310)
train_idx <- sample(1:nrow(Bike_DF),size = 0.7*nrow(Bike_DF),replace=FALSE)
Bike_train <- Bike_DF[train_idx,]
Bike_test <- Bike_DF[-train_idx,]
  

#e) Fit a forward stepwise linear model on the training data with cnt as your outcome variable, and season, holiday, month, workingday, weathersit, temp, atemp, hum, windspeed, and the four squared terms, as your predcitor varaibles. Set max number of variables to 20. Note we end up with more variables because of factor variables. Save this as an object fwd_fit.
 
library("leaps")
fwd_fit <- regsubsets(cnt ~ season + holiday + mnth + workingday + weathersit + temp + hum + windspeed,
                      data = Bike_train,
                      method = "forward",
                      nvmax = 20)
  
#f) Run the summary command over fwd_fit. What are the first five variables selected? Use the plot command to show variables selected. Hint: use scale="adjr2" to sort the models based on the adjusterd R2.
 
summary(fwd_fit)
plot(fwd_fit, scale="adjr2")
  
#g) Fit a Ridge model against the bike_train dataset. Call the plot function against the fitted model to see how MSE varies as we move λ.
 
library("glmnet")
library("glmnetUtils")
ridge_fit <- cv.glmnet(cnt ~ season + holiday + mnth + workingday + weathersit + temp + hum + windspeed,
                       data = Bike_train,
                       alpha = 0,
                       nfolds = 10)
plot(ridge_fit)
  

#h) What are the values for lambda.min and lambda.1se? What is the meaning of each of these lambdas?
  
  #lambda.min = 118.0217
  #lambda.1se = 476.4557
  
  #These lambdas decide how much we care about bias vs variance. Because our lambdas are pretty big, overall we are choosing to increase our bias a little bit in exchange for less variance.
  
  
#i) Print the value of the coefficients at lambda.min and lambda.1se. What do you notice about the differenecs between the coefficients. (Note: you will need to type as.matrix(coef(ridge_fit, s = "lambda.min")) to convert the coefficient vector from a sparse data matrix to a matrix.
                                                                                                                                            
lasso_mod <- cv.glmnet(cnt ~ season + holiday + mnth + workingday + weathersit + temp + hum + windspeed,
                       data = Bike_train,
                       alpha = 1,
                       nfolds = 10)
  
#k) How many variables are selected by the lambda.min and lambda.1se versions of the model? Print the coefficient vectors for each.
 
coef(lasso_mod, lasso_mod$lambda.1se)
coef_mat_1se <- data.frame(rownames = rownames(coef(lasso_mod)) %>%   data.frame(),coef_1se <- as.matrix(coef(lasso_mod, lasso_mod$lambda.1se)) %>% round(3)) %>% remove_rownames() %>% rename(rownames = 1,coef_1se = 2)

coef(lasso_mod, lasso_mod$lambda.min)
coef_mat_min <- data.frame(rownames = rownames(coef(lasso_mod)) %>% data.frame(),coef_1se <- as.matrix(coef(lasso_mod, lasso_mod$lambda.min)) %>% round(3)) %>% remove_rownames() %>% rename(rownames = 1,coef_1se = 2)
  
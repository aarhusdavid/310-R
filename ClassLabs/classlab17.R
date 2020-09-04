rm(list = ls()) #removing all variables


#######################################
#                                     #
#           Model Selection           #
#                                     #
#              Class 17               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - What best subset selection is
#   - Why best subset selection is infeasible for most datasets
#   - Forward stepwise algorithm
#   - Programming a forward stepwise model using "leaps" package
#   - Backward stepwise algorithm
#   - Programming backward stepwise model 
#   - Pros and cons of forward stepwise
#   - Ridge regression equation  
#   - Lambda in ridge regression
#   - How ridge regression trades off bias for variance
#   - Estimating ridge model with glmnet and glmnetUtils
#   - Lasso regression equation 
#   - How Lasso acts as a variable selection mechanism
#   - Estimating Lasso models in OLS
#   - How to print Lasso coefficients for different lambdas using the coef() function 
#   - Examining shrinkage path using coefpath
#   - Graphical representation of Lasso penalty
#   - When to use Ridge versus Lasso
#######################################


#------------------------------------------------
### Setup
#------------------------------------------------
# load the movies dataset and clean the data
options(scipen = 50)
# make sure your working directory is set right.
movies <- read.csv("Datasets/movie_metadata.csv")
movies <- movies[complete.cases(movies),]
movies <- movies[movies$budget < 400000000,]
movies <- movies[(movies$content_rating != "" & movies$content_rating != "Not Rated"),  ]
movies$grossM <- movies$gross/1e+6
movies$budgetM <- movies$budget/1e+6
movies$profitM <- movies$grossM - movies$budgetM
movies$genre_main <- do.call('rbind',strsplit(as.character(movies$genres), '|', fixed=TRUE))[,1]
library(forcats)
movies$genre_main <- fct_lump(movies$genre_main,5)
movies$content_rating <- fct_lump(movies$content_rating,3)
movies$country <- fct_lump(movies$country,2)
movies$cast_total_facebook_likes000s <- movies$cast_total_facebook_likes / 1000

# subset the data for the columns needed for the following regression
# profitM ~ all variables except: director_name,actor_1_name,
#                                 actor_2_name,actor_3_name,
#                                 plot_keywords,movie_imdb_link,
#                                 country,budgetM,grossM, genres,
#  
movies_sub <- subset(movies, select=-c(director_name,actor_1_name,
                                       actor_2_name,actor_3_name,
                                       plot_keywords,movie_imdb_link,
                                       country,budgetM,grossM, genres,
                                       language, movie_title, budget, gross))
set.seed(310)
train_idx <- sample(1:nrow(movies_sub),size = floor(0.75*nrow(movies_sub)))
movies_train <- movies_sub[train_idx,]
movies_test <- movies_sub[-train_idx,]

# check the dimension


#------------------------------------------------
### Forward Stepwise Selection
#------------------------------------------------
# install.packages('leaps')
library("leaps")
# train a forward stepwise model (use # of max variables = 20)
forward_mod <- regsubsets(profitM ~ .,
                          data = movies_train,
                          nvmax = 25,
                          method = "forward")


# summary of the model
summary(forward_mod)

# plot what variables are selected at each stage
plot(forward_mod, scale = "adjr2")
# extract variables are selected
reg.summary <- summary(forward_mod)
reg.summary$which

# extract the adjr2 for models from 1 to 20 variables
reg.summary$adjr2

# plot adjusted R2 against the # of variables in the model
plot(reg.summary$adjr2, ylab = "Adjusted Rsquared", type = "l", col = "red")

#------------------------------------------------
### Backward Stepwise Selection
#------------------------------------------------
backward_mod <- regsubsets(profitM ~ .,
                           data = movies_train,
                           nvmax = 25,
                           method = "backward")


# plot the variables selected
plot(backward_mod, scale = "adjr2")


#------------------------------------------------
### Ridge Regression
#------------------------------------------------
#install.packages("glmnet")
#install.packages("glmnetUtils")
library("glmnet")
library("glmnetUtils")


# estimate ridge mod 

ridge_mod <- cv.glmnet(profitM ~ .,
                       data = movies_train,
                       alpha = 0) #use alpha = 0 for ridge
# print the coefficients of the Ridge model
coef(ridge_mod)

# plot the MSE against different values of lambda
plot(ridge_mod)

# best lambda
ridge_mod$lambda.min

# lambda lse
ridge_mod$lambda.1se


# explore how coefficients change as we change lambda
# install.packages("coefplot")
library("coefplot")

coefpath(ridge_mod)
# don't use the coefplot in markdown or test!



#------------------------------------------------
### Lasso Regression
#------------------------------------------------

# estimate Lasso mod 

lasso_mod <- cv.glmnet(profitM ~ .,
                       data = movies_train,
                       alpha = 1)

# print the coefficients for lambda.min and lambda.1se
coef(lasso_mod, s = lasso_mod$lambda.min)
coef(lasso_mod, s = lasso_mod$lambda.lse)


# put in a matrix



# plot lasso mse for different values of lambda
plot(lasso_mod)

# explore how coefficients 
# change as we change lambda
coefpath(lasso_mod)


#------------------------------------------------
### A Note on glmnet for Classification
#------------------------------------------------

# If you want to use Ridge and Lasso for a classification
# problme, you just need to enter family=binomial.

# for logit
# family = binomial
# type.measure = 'auc'


#######################################
#                                     #
#           Model Selection           #
#                                     #
#              Class 16               #
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
dim(movies_train)

#------------------------------------------------
### Forward Stepwise Selection
#------------------------------------------------
# install.packages('leaps')
library("leaps")

# train a forward stepwise model (use # of max variables = 20)
forward_mod <- regsubsets(profitM ~ .,
                         data = movies_train,
                         nvmax = 20,
                         method = "forward")

# summary of the model
summary(forward_mod)
# plot what variables are selected at each stage

# extract variables are selected


# extract the adjr2 for models from 1 to 20 variables

# plot adjusted R2 against the # of variables in the model


#------------------------------------------------
### Backward Stepwise Selection
#------------------------------------------------



# plot the variables selected



#------------------------------------------------
### Ridge Regression
#------------------------------------------------
#install.packages("glmnet")
#install.packages("glmnetUtils")



# estimate ridge mod 


# print the coefficients of the Ridge model


# plot the MSE against different values of lambda


# best lambda


# explore how coefficients change as we change lambda
# install.packages(coefplot)

# don't use the coefplot in markdown or test!

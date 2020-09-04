#######################################
#                                     #
#   OLS: Prediction and Diagnostics   #
#                                     #
#              Class 10               #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   OLS
#   - Partition data into test and train
#   - Run linear regression using lm()
#   - Interpret the results of lm
#   - Plot the linear model
#   - Non-linear transformation
#   - Prediction
#   - Measuring Training and Test errors (RMSE, MSE)
#   - Residuals/Predictions/True values and plots
#   - Linear regression diagnostics
#######################################

# libraries
library(ggplot2)
library(forcats)
library(caret)
library(corrplot)
library(margins)
library(coefplot)
library(doBy)

#######################################
# 1. Reading Data
#######################################

# load nba_players.csv data
nba <- read.csv("Datasets/nba_players.csv")

# Here is the description of the variables:

# 1)  X:                 index
# 2)  player_name:       Name of the player
# 3)  team_abbreviation: Abbreviated name of the team the player played for (at the end of the season)
# 4)  age:               Age of the player
# 5)  player_height:     Height of the player (in centimeters)
# 6)  player_weight:     Weight of the player (in kilograms)
# 7)  college:           Name of the college the player attended
# 8)  country:           Name of the country the player was born in (not necessarily the nationality)
# 9)  draft_year:        The year the player was drafted
# 10) draft_round:       The draft round the player was picked
# 11) draft_number:      The number at which the player was picked in his draft round
# 12) gp:                Games played throughout the season
# 13) pts:               Average number of points scored
# 14) reb:               Average number of rebounds grabbed
# 15) ast:               Average number of assists distributed
# 16) net_rating:        Team's point differential per 100 possessions while the player is on the court
# 17) oreb_pct:          Percentage of available offensive rebounds the player grabbed while he was on the floor
# 18) dreb_pct:          Percentage of available defensive rebounds the player grabbed while he was on the floor
# 19) usg_pct:           Percentage of team plays used by the player while he was on the floor (FGA + Possession Ending FTA + TO) / POSS)
# 20) ts_pct:            Measure of the player's shooting efficiency that takes into account free throws, 2 and 3 point shots (PTS / (2*(FGA + 0.44 * FTA)))
# 21) ast_pct:           Percentage of teammate field goals the player assisted while he was on the floor
# 22) season:            NBA season 


#######################################
# 2. Feature Transformation
#######################################

# Remove odd draft rounds and keep 1,2, and Undrafted
nba <- nba[nba$draft_round %in% c("Undrafted","1","2"),]
nba$draft_round <- factor(nba$draft_round)
# Create a USA dummy variable
nba$USA <- ifelse(nba$country=="USA",1,0)
# Create BMI variable (Body Mass Index: weight/height^2)
nba$bmi <- nba$player_weight/((nba$player_height/100)^2)
# lump colleges together and keep the 5 most frequent ones
# use fct_lump from forcats library
nba$college_simple <- fct_lump(nba$college, n = 5)

#######################################
# 3. Correlation structure
#######################################

# Let's calculate the (pairwise) correlation among the variables: 
#    player_height, player_weight, gp, pts, reb, ast, net_rating
cor(nba[,c(5,6,12:16)])
# That would be hard to read. Let's use a visualization tool.
# Use "corrplot" package. If you don't have it, install.packages("corrplot")
corrplot(cor(nba[,c(5,6,12:16)]))


#all the columns of data
cor(nba)
col_numeric <- sapply(nba, FUN = is.numeric)
col_numeric

cor(nba[ ,col_numeric])
corrplot(cor(nba[ ,col_numeric]))
#######################################
# 4. Partition data into train and test
#######################################

# Now, we want to make our train and test sets
# Split the data by 75% train and 25% test
# Simple method of testing/training split
# very important to set seed! use set.seed(310)
# get number of obs and use sample() with no replacement to pick the row index
set.seed(310)
train_indx <- sample(1:nrow(nba), 0.75*nrow(nba), replace=FALSE)
nba_train <- nba[train_indx,]
nba_test <- nba[-train_indx,]

# check the number of rows for train and test
nrow(nba_train)
nrow(nba_test)

#######################################
# 5. Linear Regression
#######################################

# Simple linear regression (just one variable)
# Model 1: regress net_rating on pts (for train data)
model1 <- lm(net_rating ~ pts, nba_train)
# use summary to see the results
summary(model1)
# how do you interpret the results?

# create a scatter plot with fitted line (net_rating ~ pts)
ggplot(nba, aes(pts, net_rating))  +geom_point() + 
  geom_smooth(method='lm') + xlim(0,15) + ylim(-50,50)

ggplot(nba, aes(pts, net_rating))  +geom_point() + 
  geom_smooth() + xlim(0,15) + ylim(-50,50)

# Model 2: regress net_rating on pts and pts^2 (for train data)
model2 <- lm(net_rating ~ pts + I(pts^2), nba_train)
# use summary to see the results
summary(model2)
# how do you interpret the results?

# margins library 
# install.packages("margins")
library("margins")
m <- margins(model2, at=list(pts=seq(0,20,2))) 
m

cplot(model2, x = "pts", what = "prediction", scatter = TRUE)

#what if we apply margin on linear model
margins(model1, at = list(pts=seq(0,20,2)))
# Multiple linear regression (more variables)
# regress net_rating on draft_round, USA, pts, reb, and ast
table(nba$USA)
table(nba$draft_round)

model3 <- lm(net_rating ~ pts + reb + ast + bmi + USA + draft_round,
             nba_train)

summary(model3)
# what variables are statistically significant at 95% level?

#changes the baseline to Undrafted
model4 <- lm(net_rating ~ pts + reb + ast + bmi + USA + 
               relevel(draft_round, ref = "Undrafted"),
             nba_train)

summary(model4)
# We can use coefplot package to visually see the magnitudes of the effect.

library("coefplot")
coefplot(model3, predictors = c("pts",
                                "reb",
                                "ast",
                                "bmi",
                                "USA"), sort = "magnitude")
# Predictions


# MSE for train and test


# Collinearity


# heterokedasticity



#######################################
# 6. Your Turn
#######################################

###Average performance by team
# We want to check the average net_rating by each team
# Recall we can use summaryBy() from doBy package

# store the results from summaryBy() in a dataframe and plot it


###Linear Regression
# Regress pts on height, weight, and bmi


# what do you find?

# create predictions for both test and training sets


# store the results in separate dataframes along with the residuals and true outcome


# calculate MSE and RMSE for both train and test. What can you say about these errors?


# plot the residual vs predicted: is it homoskedastic or heteroskedastic?

# VIF measure. Do we see any multicollineartiy?


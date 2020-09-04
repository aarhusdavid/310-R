rm(list = ls()) #removing all variables

#######################################
#                                     #
#    Analyzing NBA Players' Stats     #
#                                     #
#              Class 9                #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   Data Exploration
#   - Check for missing values
#   - Identify and remove outliers
#   - Correlation between variables
#   - Correlation map using corrplot()
#   - Calculate and plot average measures per team
#   - fct_lump function for lumping factor levels
#   - Using frequency table
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

#######################################
# 1. Reading Data
#######################################

# We are using a dataset on NBA players' stats from 1996 to 2016
# It is from Kaggle website. You can download the data from here:
# https://www.kaggle.com/justinas/nba-players-data
# Dataset is also available from Canvas. Download it and put
#  it in your working directory. The file name is "nba_players.csv"

# First things first: set the working directory
# (Alternatively, you can use R project to set the working director)

# Now, open the data set using read.csv() and store it in 'nba' dataframe
setwd("/Users/DavidAarhus/Documents/310 R/Datasets")
nba <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/nba_players.csv")
# Great! now, let's check what the data is about

# What is its dimension?
dim(nba)
# So there are 9561 records (obs) in this data!
# Check out the variables names

# Cool! Here is the description of the variables:

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

# Now, take a look at the data
View(nba)
# Check the types of the variables
str(nba)
# there are 12 numeric variables

# how many unique players are there?
length(unique(nba$player_name))
#######################################
# 2. Data Cleaning
#######################################
sum(is.na(nba))
# Check how many missing variables are there

# it seems we are in luck, and there is no missing data.

# Draft round
# what are the draft round levels
levels(nba$draft_round)
# make a box plot of pts over draft round
library("ggplot2")
ggplot(nba, aes(draft_round, pts)) +
    geom_boxplot()
# anything odd? usually there are two rounds of drafting in nba
# count the draft numbers not in 1,2,undrafted
# note that draft_round is stored as factor
# use %in% operator to check whether a value is in a set
sum(!nba$draft_round %in% c("1","2","Undrafted"))
# 44 observations are not in that set. That might be a data anomaly.
# Remove them from the data set
nba <- nba[nba$draft_round %in% c("1","2","Undrafted") , ]
nba$draft_round <- factor(nba$draft_round)
dim(nba)
# Explore the origin countries of the players
table(nba$country)
# Create a dummy variable for american players
sum(nba$country == "USA")

nba$USA <- ifelse(nba$country == "USA", 1, 0)

table(nba$USA)
# Create a violin plot for player height for american and non-american players
ggplot(nba, aes(factor(USA), player_height, fill = factor(USA))) +
  geom_violin() +
  theme_minimal() +
  labs(fill = "USA", x = "American", y = "Points")

# Creating BMI variable (Body Mass Index: weight/height^2)
nba$BMI <- nba$player_weight / (nba$player_height/100)^2

# Create a scatter plot weight against height with color = bmi
ggplot(nba, aes(player_height, player_weight, color = BMI )) +
  geom_point()

# Is rebound a function of height or skills

# Explore colleges
table(nba$college)
# lump colleges together and keep the 5 most frequent ones
# use fct_lump from forcats library
install.packages("forcats")
library("forcats")

nba$college_simple <- fct_lump(nba$college, n = 5)

table(nba$college_simple)


# Create a frequency table for college and draft
table(nba$college_simple, nba$draft_round)

freq_table <- table(nba$college_simple, nba$draft_round)

prop.table(freq_table, margin = 2)
#######################################
# 3. Correlation structure
#######################################

# Let's calculate the (pairwise) correlation among the variables: 
#    player_height, player_weight, gp, pts, reb, ast, net_rating

# That would be hard to read. Let's use a visualization tool.
# Use "corrplot" package. If you don't have it, install.packages("corrplot")



#######################################
# 4. Partition data into train and test
#######################################

# Now, we want to make our train and test sets
# Split the data by 75% train and 25% test
# Simple method of testing/training split
# very important to set seed! use set.seed(310)
# get number of obs and use sample() with no replacement to pick the row index
set.seed(310)
train_index <- sample(1:nrow(nba), size = 0.75*nrow(nba),
                      replace = FALSE)
nba_train <- nba[train_index, ]
nba_test <- nba[-train_index, ]
# check the number of rows for train and test
nrow(nba_train)
nrow(nba_test)
#######################################
# 5. Linear Regression
#######################################

# Simple linear regression (just one variable)
model1 <- lm(net_rating ~ pts, nba_train)
# Model 1: regress net_rating on pts (for train data)
summary(model1)
# use summary to see the results

# how do you interpret the results?

# create a scatter plot with fitted line (net_rating ~ pts)
ggplot(nba, aes(pts, net_rating)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE) +
  xlim(0,15) +
  ylim(-50,50)

# Model 2: regress net_rating on pts and pts^2 (for train data)
model2 <- lm(net_rating ~ pts + I(pts^2), nba_train)
# use summary to see the results
summary(model2)
# how do you interpret the results?

# margins library 
install.packages("margins")
library("margins")

m <- margins(model2, at = list(pts = seq(0,20,2)))
m
# Multiple linear regression (more variables)
# regress net_rating on draft_round, USA, pts, reb, and ast


# what variables are statistically significant at 95% level?


# We can use coefplot package to visually see the magnitudes of the effect.


# Predictions



#######################################
# 6. Your Turn
#######################################

### Popular Colleges
# Let's find out what colleges are the most productive ones

# Now, we want to sort the results. First, we store that results in a dataframe

# create the sorted index using order (make the order descending)


# show the first 10 colleges


###Average performance by team
# We want to check the average net_rating by each team
# Recall we can use summaryBy() from doBy package

# store the results from summaryBy() in a dataframe and plot it

# use ggplot to produce the same plot


###Linear Regression
# Regress pts on height, weight, and bmi


# what do you find?
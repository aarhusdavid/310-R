rm(list = ls()) #removing all variables

#Question a
movies <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/movie_metadata.csv") #loads dataset

#Question b
# removing missing values of budget and gross
movies <- movies[!is.na(movies$budget),] 
movies <- movies[!is.na(movies$gross),]
#removes movies with ridiculous budgets
movies <- movies[movies$budget<4e+8,]
#puts gross, budget, and profit columns in terms of 'millions'
movies$grossM <- movies$gross/1e+6
movies$budgetM <- movies$budget/1e+6
movies$profitM <- movies$grossM-movies$budgetM 
movies$cast_total_facebook_likes000s <- movies$cast_total_facebook_likes / 1000
#creates training and test datasets
set.seed(310)
train_indx <- sample(1:nrow(movies), 0.8 * nrow(movies), replace=FALSE) 
movies_train <- movies[train_indx, ]
movies_test <- movies[-train_indx, ]

#Question c
nrow(movies_train) # gives number of rows
nrow(movies_test) # gives number of rows

#Question d
nums <- sapply(movies, is.numeric) # names of numeric variables 
cormat <- cor(movies[,nums], use="complete.obs") #removes missing values from correlation matrix
print(cormat[,"profitM"]) #prints the correlation coefficient between profitM and all the numeric varibles in the data frame

#Question e
library("corrplot")
corrplot(cormat) #prints of correlation plot 

#Question f
#create model
mod1 <- lm(profitM ~ imdb_score + cast_total_facebook_likes000s, movies_train)
#summary of mod1
summary(mod1)

#Question g
# cast_total_facebook_likes000s show that it is positively correlated with ProfitM. 
# This means that for every 0.2532 units of cast_total_facebook_likes000s ProfitM increases 1 unit

#Question h
# imdb_score p-value < 2e-16
# cast_total_facebook_likes000s p-value < 2e-16
# p-value represents how significant each variable is to profitM
# in this case both p-values are very low, therefore they are both significant

#Question i 
# imdb_score has a high coefficent as well as a low p-value, this means that this variable is significant
# and has a large affect on profitM
# the imdb_score and cast_total_facebook_likes000s variables are statistically significant 
  # at a 95% level because their p-value is < 0.05.

#Question j
# our R^2 was 0.07812, this implies our model is not accurate nor a good fit to the data
# our Adjusted R^2 was 0.07752

#Question k
# F-statistic: 131.3 on 2 and 3100 DF
# This represents the probability of all the coefficents being zero at the same time.

#Question l
mod1$residuals
length(mod1$residuals)
nrow(movies_train)

#Question m
# did not attempt

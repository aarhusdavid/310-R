rm(list = ls()) #removing all variables

#Question 1a
movies <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/movie_metadata.csv") #loads dataset

#Question 1b
names(movies) #prints the names of all the columns

#Question 1c
missingvalues <-  sum(is.na(movies$budget)) #counts missing values
missingvalues
movies <- movies[!is.na(movies$budget),] #removes missing balues in budget
dim(movies) #lists the dimensions of the new movies dataset

#Question 1d
length(unique(movies$director_name, incomparables = FALSE)) #counts the amount of unique directors in the dataframe

#Question 1e
library("ggplot2") #loads ggplot library 
ggplot(movies, aes(imdb_score, budget)) + geom_point() #prints off scatterplot

#Question 1f
movies <- movies[movies$budget<400000000,] #removes rows with movie budgets over 400m
nrow(movies) #4539 movies in data set

#Question 1g
ggplot(movies, aes(imdb_score, budget)) + 
  geom_point() +
  geom_smooth(method = 'lm') #creates linear trendline for imdb and budget

#there is no definitive explanation for a relationship between the two variables.
#Only a slight positive slope in the trendline. 

#Question 1h
ggplot(movies, aes(imdb_score, budget)) + 
  geom_point() +
  geom_smooth(method = 'lm') +
  facet_wrap(~content_rating, scales = "free")

#if we are looking soley at relationship strength,
#TV-G and NC-17 have a strong negative relationship.
#However they do not have alot of data points. 
#If the amount of data points matter, PG-13 has the strongest (positive) relationship


#Question 2
# to create budget and gross columns in millions
movies$grossM <- movies$gross/1e+6 
movies$budgetM <- movies$budget/1e+6 # note how we created new columns
# to create a column for main genre
movies$genre_main <- do.call('rbind',strsplit(as.character(movies$genres), '|', fixed=TRUE))[,1]

#Question 2a
movies$profitM <- movies$grossM - movies$budgetM #creates profit margin
movies$ROI <- movies$profitM/movies$budgetM #creates ROI margin

#Question 2b
mean(movies$ROI, na.rm= TRUE) #average ROI

#Question 2c
hist(movies$ROI) #creates histogram for ROI in movie dataset

#Question 2d
sum(movies$ROI > 10, na.rm = TRUE) #counts movies with ROI greater than 10
movies <- movies[movies$ROI<10,] #removes Movies with ROI greater than 10

#Question 2e
ggplot(movies, aes(ROI, fill = genre_main)) + 
  geom_histogram() #new histogram

#Question 2f
library("doBy")
summaryBy(ROI ~ genre_main, movies, FUN = mean) #creates a summary that gives the mean ROI for each film genre
genre_mean <- summaryBy(ROI ~ genre_main, movies, FUN = mean) #assigns mean genre list to an object
max(genre_mean[,2], na.rm= TRUE ) #gives highest ROI
which(genre_mean$ROI.mean == max(genre_mean[,2], na.rm = TRUE )) #identifies the row of which genre has the highest ROI mean
genre_mean[which(genre_mean$ROI.mean == max(genre_mean[,2], na.rm= TRUE )),] #identifies the genre name
#Musical genres have the highest ROI

#Question 2g
ggplot(genre_mean, aes(ROI.mean, genre_main, color = genre_main)) +
  geom_point() #creates scatterplot that shows the variety in mean ROI amongst genres

#Question 3a
set.seed(310)
train_idx <- sample(1:nrow(movies), size = 0.80*nrow(movies), replace = FALSE) 
movies.training <- movies[train_idx, ]
movies.test <- movies[-train_idx, ]

#Question 3b
dim(movies) #checks the dimensions
dim(movies.training)
dim(movies.test)

#Question 3c
mod1 <- lm(profitM ~ imdb_score, movies.training) #estimating our model using the training dataset
summary(mod1)

#Question 3d
coef(mod1)
#These coefficents show that as the imdb score for a film increases, 
#the profit of the Movie will also increase


  

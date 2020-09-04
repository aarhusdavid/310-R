rm(list = ls()) #removing all variables

#Question 1a
college <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/college.csv")

#Question 1b
rownames(college) = college[,1] #established the rownames for each college
head(college)

college <- college[,-1] #deletes the 1st column in the dataset since it 
                          #is being used to identify the rows
head(college)

#Question 1c
summary(college) #function to produce a numerical summary of 
                  #the variables in the data set

#Question 1d
pairs(college[ , 1:10]) #a scatterplot matrix of the first ten columns 
                          #or variables of the
                          #data. 

#Question 1e
plot(college$Private, college$Outstate) #function to produce side-by-side boxplots of 
                                          #Outstate versus Private

#Question 1f
Elite <- rep("No",nrow(college))       #  Creates new Elite variable
Elite[college$Top10perc >50] <- "Yes"  #  whether or not the proportion of students coming from the
Elite <- as.factor(Elite)                 #  top 10% of their high school classes exceeds 50%.
college <- data.frame(college, Elite)  #  rerunning the college dataframe

summary(college) #gives dataframes statistics
plot(college$Elite, college$Outstate) #produces boxplot for Outstate and Elite

#Question 1g
par(mfrow=c(2,2))
hist(college$Top10perc)
hist(college$Enroll)
hist(college$Top25perc)
hist(college$Books)

#Question 1h
par(mfrow=c(1,1))
plot(college$Apps, college$Enroll)
#as applications for colleges rise so does the Enroll
plot(college$Apps, college$Accept)
#more apps equal more Acceptions
hist(college$Enroll)
#most schools enroll below 1000 students
hist(college$Room.Board)
#This shows a wide range between room and board for universities, with the average
#coming in around $4500

#Question 2a
library("MASS")# Now the data set is contained in the object Boston.
head(Boston)# Read about the data set by ?Boston
nrow(Boston) #gets number of rows
ncol(Boston) #gets number of columns
Boston <- data.frame(Boston) #the columns give information about the population at Boston
  #crime, indus, age, tax, race, etc
colnames(Boston)
?Boston

#Question 2b
library("corrplot") #loads correlation plot availability 
corrplot(cor(Boston[,])) #creates correlation plot
#proportion of non-retail business acres per town and full-value property-tax rate per \$10,000.
  #highly correlated
#index of accessibility to radial highways and per capita crime rate by town.
  #highly correlated

#Question 2c
#yes, the index of accessibility to radical highways is correlated to the crime rate 
  #also, full-value property-tax rate per \$10,000 is correlated to crime rate

#Question 2d
# The proportion of non-retail business acres per town have a correlation with high taxes. (~0.6 - 0.8)
# Also there is an interesting correlation between lower status of the population and high tax rate (~0.4 - 0.6)
# The proportion of non-retail business acres per town also has high correlation with high tax rates (~0.6 - 0.8)
# Pupil-teacherratios is correlated with tax and rad (~0.2 - 0.4)

#Question 2e
sum(Boston[,4]) #35 uburbs in this data set bound the Charles river

#Question 2f
median((Boston[,4])) #median is 0

#Question 2g
min(Boston[,14]) #takes lowest median value of owner-occupied homes
# min median: 5 )

(Boston[399,]) #lists values of all the predictors for the lowest median of owner-occupied homes
range(Boston[,1])
(Boston[399,1])
range(Boston[,2])
(Boston[399,2])
range(Boston[,3])
(Boston[399,3])
range(Boston[,4])
(Boston[399,4])
range(Boston[,5])
(Boston[399,5])
range(Boston[,6])
(Boston[399,6])
range(Boston[,7])
(Boston[399,7])
range(Boston[,8])
(Boston[399,8])
range(Boston[,9])
(Boston[399,9])
range(Boston[,10])
(Boston[399,10])
range(Boston[,11])
(Boston[399,11])
range(Boston[,12])
(Boston[399,12])
range(Boston[,13])
(Boston[399,13])
range(Boston[,14])
(Boston[399,14])
 # functiions above list the ranges for all the predictors in the dataset
 # Then I listed the value of the indicator with the min median Owner-occupied value
 # Almost all the predictors were higher than most of the other observations except, zn, dis, and medv
 # This row had the highest Bk proportion 


#Question2h 
sum(Boston$rm > 7) #how many of the suburbs average more than seven rooms per dwelling
sum(Boston$rm > 8) #how many of the suburbs average more than eight rooms per dwelling
Boston[ Boston$rm > 8 , ] #list values for suburbs that average more than eight rooms per dwelling
#I noticed that all of them have high bk values and low lstat values.  





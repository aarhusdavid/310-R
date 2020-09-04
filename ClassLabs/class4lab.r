rm(list = ls()) #removing all variables

#swirl package is another tool to practice R

#dataframes

#example
my.df <- data.fram(id = 1:5,
                   color = c(B,G,G,G,B),
                   year = 2000:2004)

#######################################
#                                     #
#         R Data Manipulation         #
#                                     #
#              Class 4                #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - R project
#   - Installing and loading packages
#   - Creating a dataframe from vectors
#   - Loading datasets from files and packages
#   - Dataframe subsetting
#   - Missing values
#   - Correlation
#   - R basic graphic tools: plot(), hist(), ...
#   - Understading ggplot2 grammar
#######################################

#######################################
# Tip:
#   To comment multiple lines use: Ctrl + Shift + C
#                           (Mac): Command + Shift + C
#######################################


#######################################
### Working directory

# check the current working directory

# use either R project or setwd() to set the working directory

#######################################
### Dataframes

# there are different ways of creating a dataframe:
#   1. creating a dataframe from vectors
#   2. using built-in or from a package
#   3. coercing a matrix or table into a dataframe
#   4. reading from a file

# first let's create a dataframe combining vectors
# we use age and wage that we created before
set.seed(310)
age <- runif(100,20,70)
wage <- rnorm(100,20,3)
indx <- 1:100
# create a dataframe with columns: indx, age, wage
wage.data <- data.frame(id = indx,
                        age = age,
                        income = wage)
# explore the dataframe
str(wage.data) 
head(wage.data) #sees the first rows
tail(wage.data) #sees the last rows
# check the dimension of the dataframe
length(wage.data) #shows columns
dim(wage.data) #shows rows and columns
nrow(wage.data) #shows rows
ncol(wage.data) #shows columns

wage.data$age #grabs the age column of the dataframe
#######################################
### Subsetting a dataframe

# access the age column by $
names(wage.data)
wage.data$age
# recall to index a dataframe
#         df[row, column]
# First column
wage.data[,1]
# First row
wage.data[1,]
# First and fourth row
wage.data[c(1,4),]
# Columns age and income
wage.data[ , 2:3]
wage.data[, c("age","income")]
# Rows 1 to 5 | Columns 1 and 3
wage.data[1:5,c(1,3)]
# Changing the value in row 1 and column 2 to 35
wage.data[1,2] <- 35
## Extracting elements by logical conditions
wage.data$age > 30
wage.data[ wage.data$age > 30 , ]
## Combining conditions
wage.data[ wage.data$age > 28 & wage.data$income < 15 ,  ]
df2 <- wage.data[ wage.data$age > 28 & wage.data$income < 15 ,  ]
dim(df2)
#######################################
### In-Class Exercis 2
patients <- data.frame(
  id = c(31, 62, 50, 99, 53, 75, 54, 58, 4, 74),
  age = c(12, 18, 20, 17, 14, 8, 12, 24, 24, 21),
  sex = c("M", "F", "F", "M", "F", "M", "M", "F","F", "M") )

head(patients,n=2)
summary(patients)
#Use a logical operator to display ages that are larger than 20
patients[patients$age > 20, "age" ]
#Do the same as above but also display the corresponding id and sex
patients[patients$age > 20, c("id","age") ]
#Display only female observations
patients[patients$sex == "F", ]
#Change the 7th age in patients from 12 to 21
patients[7,"age"] <- 21
#Calculate the proportion of subjects that are age 20 or greater
sum(patients$age >= 20) /nrow(patients)
#Calculate the proportion of males that are greater than 20
sum(patients$age > 20 & patients$sex=="M") / nrow(patients)
#Permanently delete the 10th subject
patients <- patients[-10, ]
patients
#Add the column year randomly selected from 2010 to 2020 to the patients dataframe.
set.seed(310)
patients$year <- sample(2010:2020,
                           nrow(patients),
                           replace=TRUE)

#######################################
### Dataframe from a csv file

# csv is a comma separated file (similar to excel data)
getwd()
# load University.csv data and store it in University
University <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/University.csv")

str(University)
head(University)
summary(University)

# checking for missing values in University
is.na(University)
# sum of missing values
sum(is.na(University))
# assign some NA (missing) to patients
patients[2,2] <- NA
patients[2,3] <- NA
patients[4,2] <- NA

# how many missing entries are there in patients?
sum(is.na(patients))
# check how many rows are there with missing?
complete.cases(patients)
# how many rows with no missing data
sum(complete.cases(patients))
#show rows that are complete
sum(!complete.cases(patients))

#######################################
### Dataframe from a package

# install package: "ISLR"
#   note: - you need to install a package once!
#         - use double quotations inside the install.packages() function
#install.packages("ISLR")
# now load the library
library("ISLR")
# load the Auto data
data(Auto)
# check the documentation
?Auto
# explore the dataset
str(Auto)
head(Auto)

# check the correlation structure
cor(Auto) #will get error because last column is not numeric
str(Auto)
cor(Auto[,-9]) #shows every column except the last one
# install the package corrplot
#install.packages("corrplot")
# use corrplot to create correlation plot
library("corrplot")
corrplot(cor(Auto[,-9]))
#######################################
### R basic graphics

# create a scatter plot of mpg against horsepower

# check plot documentation for options

# change the plot title and axes names and the colors and a vertical line

# how to save a plot?

# create a boxplot mpg over origin

#######################################
### ggplot
# ggplot is a more advanced approach to R graphics
# It uses ggplot(df) function plus layers that can go on the main layer.
# aes: you can set up main aesthetic features of the plot: x, y, color, ...
# This this the reference guide on layers:
# https://ggplot2.tidyverse.org/reference/
library(ggplot2)

# using University dataframe plot a scatterplot of GPA against GRE

# plot a scatter plot of GPA against GRE with different colors for department

# plot a scatter plot of GPA against GRE with different colors for admitted

# plot a scatter plot of GPA against GRE with a smoothing line

# plot a scatter plot of GPA against GRE with a linear smoothing line

# plot multiple scatter plots of GPA against GRE for different departments

# what happens if you add one smoothing line to the plot above?

# find a way to have just one smoothing line

# options:
#     - no standard error for smoothing line
#     - change size of points
#     - change color transparency
#     - change the theme
#     - location of legend
#     - plot title and axes' labels






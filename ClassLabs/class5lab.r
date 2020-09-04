
#######################################
#                                     #
#         Data Visualization          #
#                                     #
#              Class 5                #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################

#######################################
# Goals:
#   - R basic graphic tools: plot(), hist(), ...
#   - Understading ggplot2 grammar
#######################################

#######################################
### Load Auto dataframe from ISLR
library("ISLR")
data(Auto)

#######################################
### R basic graphics

# create a scatter plot of mpg against horsepower
plot(Auto$horsepower, Auto$mpg)
# check plot documentation for options
?plot
# change the plot title and axes names and the colors and a vertical line
plot(Auto$horsepower, Auto$mpg, col = 'red', main="MPG over Hp",
    xlab = "HP",
    ylab = "MPG")

# how to save a plot?
pdf("my_first_plot.pdf")
plot(Auto$horsepower, Auto$mpg)
dev.off()

# create a boxplot mpg over origin
# creates labels for orgin
Auto$origin <- factor(Auto$origin, 
                      labels=c('American','European','Japanese'))

head(Auto)

plot(Auto$origin, Auto$mpg)

#pairs
pairs(Auto)
pairs(Auto[ , 1:5])
#######################################
### Load University dataframe from csv file


#######################################
### ggplot
# ggplot is a more advanced approach to R graphics
# It uses ggplot(df) function plus layers that can go on the main layer.
# aes: you can set up main aesthetic features of the plot: x, y, color, ...
# This this the reference guide on layers:
# https://ggplot2.tidyverse.org/reference/
#install.packages("ggplot2")
library("ggplot2")

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


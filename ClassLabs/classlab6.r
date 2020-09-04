#######################################
#                                     #
#              ggplot2                #
#                                     #
#              Class 6                #
#                                     #
#              MGSC 310               #
#       Prof. Shahryar Doosti         #
#                                     #
#######################################
rm(list = ls()) #removing all variables


#######################################
### Load University dataframe from csv file

University <- read.csv("/Users/DavidAarhus/Documents/310 R/Datasets/University.csv")

head(University)


#######################################
### ggplot
# ggplot is a more advanced approach to R graphics
# It uses ggplot(df) function plus layers that can go on the main layer.
# aes: you can set up main aesthetic features of the plot: x, y, color, ...
# This this the reference guide on layers:
# https://ggplot2.tidyverse.org/reference/
library(ggplot2)

# using University dataframe plot a scatterplot of GPA against GRE
ggplot(University, aes(GRE, GPA, color = department)) +
  geom_point()

ggplot(University, aes(GRE, GPA)) +
  geom_point(aes(color = factor(admitted)))
# plot a scatter plot of GPA against GRE with different colors for department
ggplot(University, aes(GRE,GPA)) +
  geom_point(aes(color = factor(admitted))) +
  geom_smooth(method = lm) +
  facet_wrap(~department) #creates different plots for different departments


# plot a scatter plot of GPA against GRE with different colors for admitted

# plot a scatter plot of GPA against GRE with a smoothing line

# plot a scatter plot of GPA against GRE with a linear smoothing line
ggplot(University, aes(GRE,GPA)) +
  geom_point() +
  geom_smooth(method = lm) #method = lm makes like linear/flat

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

ggplot(University, aes(GRE,GPA,color = department)) +
  geom_point() +
  labs(title = "GPA vs GRE", "GRE", "GPA") +
  geom_smooth(method = lm) +#method = lm makes like linear/flat
  theme_light() +
  theme(legend.position = 'bottom')


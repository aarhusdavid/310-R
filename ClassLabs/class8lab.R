rm(list = ls()) #removing all variables

library("ISLR")
data(Auto)

Auto$origin <- factor(Auto$origin, 
                      labels = c("American", "European", "Japanese"))

summary(Auto)
summary(Auto$mpg)

library("doBy")

#calculate the mean of mpg for different origin 
#left hand side: focal variable
#right hand sie: group variable

summaryBy(mpg ~ origin, Auto, FUN = mean)
summaryBy(weight ~ origin, Auto, FUN = median)
summaryBy(. ~ origin, Auto[,-9], FUN = mean)

mpg.mean <- summaryBy(mpg ~ origin, Auto, FUN = mean)


#apply and sapply functions
apply(Auto[,-9], 2, FUN = range) #1 for row-wise and 2 for col
sapply(Auto[,-9], mean) #for columns

sapply(Auto, is.numeric) #checks of columns are numeric

#ggridge
install.packages("ggridges")
library("ggridges")
library("ggplot2")

#the histogram for mpg for different origin
ggplot(Auto, aes(mpg, origin)) +
  geom_density_ridges(aes(fill = origin))


#linear Regression
mod1 <- lm(mpg ~ weight, Auto)
summary(mod1)

mod2 <- lm(mpg ~ weight + cylinders, Auto)
summary(mod2)

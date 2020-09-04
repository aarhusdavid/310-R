
# Name: David Aarhus
# Problem Set: 1 

rm(list = ls()) #removing all variables

# Question 1a
x <- seq(-1,1,0.02)
# -1 is the starting point
# 1 is the ending point
#0.02 is the step of each element between -1 and 1

#Question 1b
y <- exp(x/2) #for every element of 'x' the element is thrown into the function
              # 'exp(x/2)' and is assigned to an indice in vector y

#Question 1c
sum(y > 1) #sums the amount of of numbers that are greater than 1

#Question 1d
ave <- mean(y) #calculates average
sum(y > ave) #sums the amount of of numbers that are greater than the average

#Question 1e
which(y > ave) #grabs the indice of each element of y that is greater than the mean

#Question 2a
#> getwd()
#[1] "/Users/DavidAarhus"
#> setwd("/Users/DavidAarhus/Documents/310 R")
#> getwd()
#[1] "/Users/DavidAarhus/Documents/310 R"

#Question 2b
id <- c(1:150) #creates a vector with 150 indices using "1:150"

#Question 2c
set.seed(310) #produces the same random results when ran with code
netflix <- rnorm(150, mean = 20, sd = 5) #creates vector of random numbers from normal distribution

#Question 2d
set.seed(310) #produces the same random results when ran with code
hulu <- runif(n=150, min=0, max=15) #creates vector of random numbers 
hulu

#Question 2e
hist(netflix) #diplays histogram for netflix
hist(hulu) #diplays histogram for hulu

#Question 2f
values <- c("Yes", "No") #creates values vector
subscription <- factor(values) #creates a factor variable
is.factor(subscription) #varifies if 'subscription' is a factor variable

#Question 2g
set.seed(310) #produces the same random results when ran with code
amazon <- sample(subscription, size = 150, replace = TRUE) #vector named amazon of size 150 
                                                           #with samples from the vector subscription
amazon[1:20] #print first 20 observations of amazon

#Question 2h
sum(amazon == "Yes") #counts to see how many amazon subscriptions are amongst the sample

#Question 2i
sum(netflix > 20 & amazon == "No") #checks to see which people watch 20+ hours of Netflix
                                   # but don't have an amazon subscription

#Question 2j
sum(hulu < 12 & netflix < 12 & amazon == "Yes") #checks to see which hulu and netflix customers
                                                # watch less than 12 hours
                                                # and also have an amazon subscription

#Question 2k
id[amazon == "Yes"] #grabs id of the users who are subscribed to amazon





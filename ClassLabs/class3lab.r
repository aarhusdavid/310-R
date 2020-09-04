#getwd() - prints the current working directory
#setwd() - set the new working directory where you usually keep files

#library(help = "datasets") #many datasets are built into R
  #for example the "islands" dataset
# >islands
  #Africa Antarctica Asia
  #1111   7000      11345
#useful arithmetic funcitons
  #min max mean median quantile war sd range
  #length() - finds number of elements in a vector
    #you can use this for the seq() function to alter elements (same with forloops)


rm(list = ls()) #removing all variables

#subsetting Vectors
  x <- c(3,4,2,1,10,7)
  x[1]  
  # [1] 3
  x[3]
  # [1] 2 
  x[1:5]
  # [1]  3  4  2  1 10
  x[c(2,5)]
  # [1]  4 10
  x[-c(2,4)]
  # [1]  3  2 10  7
  
  
#Logical Value (T or F)
passed <- TRUE
x_1 <- 2
y <- 3
# != 
# ==
# '|' or


x_1 > 0 & y > 0
# [1] TRUE

v = c(1:5)
v < 2
#[1]  TRUE FALSE FALSE FALSE FALSE



day <- c("Sun","Mon","Tues", "Wed", "Thurs", "Fri", "Sat")
rain <- c("Yes", "Yes", "Yes", "Yes", "Yes","Yes", "No")
snow <- c("No", "No", "No", "Yes", "No","No", "No")

#if it rains (or rain is equal to "Yes")
rain == "Yes"
# [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE


#Conditional Subsetting
day[rain == "Yes"]
#[1] "Sun"   "Mon"   "Tues"  "Wed"   "Thurs" "Fri" 
day[snow == "Yes"]
#[1] "Wed"

#how many days did it rain this week
sum(rain == "Yes")

#which element(s) are TRUE
which(snow == "Yes")

#any function
any(c(TRUE,FALSE,FALSE,FALSE))



k <- c(3,2,15,-1,22,1,9,17,5)

#display first and last values
k[1]
# 3
k[length(k)]
# 5

#display values that are greater than the mean of k
k > mean(k)
#[1] FALSE FALSE  TRUE FALSE  TRUE FALSE  TRUE  TRUE FALSE
k[k > mean(k)]
#[1] 15 22  9 17

#display the positions of those values above
which(k > mean(k))
#[1] 3 5 7 8

#are all values positive
all(k > 0)
#FALSE


#Random Numbers
  #Uniform runif()
  #Normal rnorm()

ran <- runif(n = 10) # random numbers between 0 and 1
rany <- rnorm(n = 10) 

age <- runif(n=100, min=18, max=70)
wage <- rnorm(100, mean = 15, sd = 2)

mean(wage)
sd(wage)

hist(wage)
hist(age, breaks = 20)

#fix the randomness

set.seed(310) #gives you the same random output as other users
runif(4)

#sample
set.seed(310)
sample(1:10,
       size = 5,
       replace = TRUE)

#replace variable allows numbers to be repeated
#[1] 9 1 4 8 3

x<-c(2,4,3,1)
which(x>3)
#factor function
genre <- c("Drama", "Sci-fi", "Thriller", "Action", "Comedy")
is.factor(genre)
#false
genre.fac <- factor(genre)
genre.fac

is.factor(genre.fac)
#true
#this is a way to give labels to your numerical values

indx <- 1:length(wage)
wage.data <- data.frame(indx, age, wage)









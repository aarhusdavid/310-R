rm(list = ls()) #removing all variables

#inclass exercise 1
x <- 1+2*(3+4)
y <- log((4^3)+3^(2+1))
j <- sqrt((4+3)*(2+1))
k <- ((1+2)/(3+4))^2

#assigning a character
firstname <- "david"
lastname <- "aarhus"

answer <- 2+2

#mode(answer) = "numeric"
#class(answer) = "numeric", and more detailed information

#is.numeric(answer) = TRUE
#checks to see if object is numeric

#typeof(answer) = "double"

#OBJECT CONVERTERS
#as.integer()
#as.character()


#VECTOR EXAMPLES
gpa <- c(3.5,3.8,4.0,2.8,3.1)

#SEQUENCE FUNCTIONS
#seq(from = 1, to = 10, by = 2)
myVector <- seq(1,10,2)
myVector2 <- 1:10
myVector3 <- seq(1,10, length.out = 5) #automatically creates step size

#CREATING VECTORS USING rep()
rep(3, times = 10) #repeates '3' 10 times
y_1 <- 1:3
rep(y, times = 4) #repeates '1,2,3' 4 times
rep(y, length = 10) #stops repeition at the end of the length




x_1 <- 1:3
log(x) #takes log of every element in the vector
y_2 <- 4:6
x_1+y_2 #adds each element to it's adjacent partner in the next vector
#same for '*','/', and '-'




#inclass Excercise

seq(2,99,2) #all the even numbers 1-99
seq(1,99,2) #all the odd numbers 1-99
c(rep(1,3),rep(2,3),rep(3,3)) #(1,1,1,2,2,2,3,3,3)
c(1:5,4:0) #1 ,2, 3, 4, 5, 4, 3, 2, 1, 0
1/1:10 #1/1,1/2,1/3,1/4,1/5/,1/6,1/7,1/8,1/9,1/10
(1:6)^3 #1,8,27,125,216


weather <- c("sunny", "sunny")
xy <- 2.3
as.character(xy)




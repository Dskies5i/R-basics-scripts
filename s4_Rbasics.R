####################################################################
####Basic Conditionals
####################################################################
# an example showing the general structure of an if-else statement
a <- 0
if(a!=0){
  print(1/a)
} else{
  print("No reciprocal for 0.")
}

# an example that tells us which states, if any, have a murder rate less than 0.5
library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000
ind <- which.min(murder_rate)
ind
if(murder_rate[ind] < 0.5){
  print(murders$state[ind]) 
} else{
  print("No state has murder rate that low")
}


# changing the condition to < 0.25 changes the result
if(murder_rate[ind] < 0.25){
  print(murders$state[ind]) 
} else{
  print("No state has a murder rate that low.")
}



# the ifelse() function works similarly to an if-else conditional
a <- 0
ifelse(a > 0, 1/a, NA)

# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

####################################################################
####Functions
####################################################################
# example of defining a function to compute the average of a vector x
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}

# we see that the above function and the pre-built R mean() function are identical
x <- 1:100
identical(mean(x), avg(x))

# variables inside a function are not defined in the workspace
s <- 3
avg(1:10)
s

# the general form of a function
my_function <- function(VARIABLE_NAME){
  perform operations on VARIABLE_NAME and calculate VALUE
  VALUE that is returned always in last line
}

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

####################################################################
####Assessment 4.1
####################################################################
##4.1: Q1

#Review the following code:
ifelse( n > 10, n, NA)

#If you set n to 4, what is the result?
n<-4
ifelse( n > 10, n,  "None")
#A:"None"

#If you set n to 13, what is the result?
n<-13
ifelse( n > 10, n,  "None")
#A:13

###4.1: Q2

#Review the following code:
x <- 25
s <- 5

test <- function(x){
  s <- 1/x
}



#What value does this code return?
#A: 0.04

#After this code is run, what is the value of x?
x
#A: 25

#After this code is run, what is the value of s?
s
5

####################################################################
####For Loops
####################################################################
# creating a function that computes the sum of integers 1 through n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}

# a very simple for-loop
for(i in 1:5){
  print(i)
}

# a for-loop for our summation
m <- 25
s_n <- vector(length = m) # create an empty vector
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}

# creating a plot for our summation function
n <- 1:m
plot(n, s_n)

# a table of values comparing our function to the summation formula
head(data.frame(s_n = s_n, formula = n*(n+1)/2))

# overlaying our function with the summation formula
plot(n, s_n)
lines(n, n*(n+1)/2)

####################################################################
####Other Functions
####################################################################

#apply
#sapply
#tapply
#mapply
#split
#cut
#reduce
#quantile
#identical
#unique

####################################################################
####Assessment 4.2
####################################################################
#4.2: Q1
#Review the following code:
test <- vector(length = 5)
for (i in 1:5){
  test[i] <- i^2
  test
}

#What is the last value in the vector test?
#A: 25

#What is the value of i after the for loop is run?
i
#A: 5

####################################################################
####DataCamp Assessment 4.0 Programming Basics
####################################################################

####Conditionals
#What will this conditional expression return? Run it from the console.
x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}
####################################################################
####Other Functions
####################################################################

#Which of the following expressions is always FALSE when at least one entry of
#a logical vector x is TRUE? You can try examples in the R console.
x<-c(TRUE, FALSE, FALSE)
all(x) #false
any(x) #true
any(!x) #true
all(!x)#false -> answer

x<-c(TRUE, TRUE)
all(x) #false
any(x) #true
any(!x) #true
all(!x)#false -> answer


##ifelse

#The function nchar tells you how many characters long a character vector is. For example:
library(dplyr)  
char_len <- nchar(murders$state)
head(char_len)
char_len
#-------------------------------------------------------------------------------
#The function ifelse is useful because you convert a vector of logicals into 
#something else. For example, some datasets use the number -999 to denote NA. 
#A bad practice! You can convert the -999 in a vector to NA using the 
#following ifelse call:

x <- c(2, 3, -999, 1, 4, 5, -999, 3, 2, 9)
ifelse(x == -999, NA, x)
#If the entry is -999 it returns NA, otherwise it returns the entry.

#INSTRUCTIONS:
#For example, where the original vector has Massachusetts (13 characters), 
#the new vector should have MA. But where the original vector has New York 
#(8 characters), the new vector should have New York as well
library(dslabs)
data(murders)
new_names<-ifelse(nchar(murders$state)>8, murders$abb, murders$state)
new_names

#-------------------------------------------------------------------------------
#Defining functions
#You will encounter situations in which the function you need does not already 
#exist. R permits you to write your own. Let's practice one such situation, 
#in which you first need to define the function to be used. 
#The functions you define can have multiple arguments as well as default values.

#To define functions we use function. For example the following function adds 1 
#to the number it receives as an argument:
my_func <- function(x){
  y <- x + 1
  y
}

#The last value in the function, in this case that stored in y, gets returned.

#If you run the code above R does not show anything. This means you defined the function. You can test it out like this:
  
my_func(5)

#INSTRUCTIONS:
#We will define a function sum_n for this exercise.

#Create a function sum_n that for any given value, say n, creates the vector 1:n, 
#and then computes the sum of the integers from 1 to n.
#Use the function you just defined to determine the sum of integers 
#from 1 to 5,000.

# Create function called `sum_n`
sum_n <-function(n){
  sum(seq(1:n))
}

# Use the function to determine the sum of integers from 1 to 5000
sum_n(5000)
#-------------------------------------------------------------------------------
#Defining functions continued...
#We will make another function for this exercise. We will define a function 
#altman_plot that takes two arguments x and y and plots the difference y-x 
#in the y-axis against the sum x+y in the x-axis.

#You can define functions with as many variables as you want. For example, 
#here we need at least two, x and y. The following function plots log transformed 
#values:

#log_plot <- function(x, y){
plot(log10(x), log10(y))
}
#This function does not return anything. It just makes a plot.

#Instructions:
#Create a function altman_plot that takes two arguments x and y and plots y-x 
#(on the y-axis) against x+y (on the x-axis).

#Note: don't use parentheses around the arguments in the plot function because 
#you will confuse R.

# Create `altman_plot` 
altman_plot<-function(x,y){
  plot(x+y,y-x)
}
#-------------------------------------------------------------------------------
#Lexical scope
#Lexical scoping is a convention used by many languages that determine when an 
#object is available by its name. When you run the code below you will see 
#which x is available at different points in the code.
x <- 8
my_func <- function(y){
  x <- 9
  print(x)
  y + x
}
my_func(x)
print(x)

#Note that when we define x as 9, this is inside the function, but it is 8 
#after you run the function. The x changed inside the function but not outside.

#INSTRUCTIONS
#After running the code below, what is the value of x?
# Run this code 
x <- 3
my_func <- function(y){
  x <- 5
  y+5
}

# Print the value of x 
x #3

#-------------------------------------------------------------------------------
#For loops
#In the next exercise we are going to write a for-loop. In that for-loop we are 
#going to call a function. We define that function here.

#Instructions

#Write a function compute_s_n that for any given Sn = 1^2 + 2^2 + 3^2 +...+ n^2
#computes the sum 
#Report the value of the sum when n=10.

# Here is an example of a function that adds numbers from 1 to n
example_func <- function(n){
  x <- 1:n
  sum(x)
}
# Here is the sum of the first 100 numbers
example_func(100)

# Write a function compute_s_n with argument n that for any given n computes 
#the sum of 1 + 2^2 + ...+ n^2

#given n computes the sum of 1 + 2^2 + ...+ n^2
compute_s_n <- function(n){
  x<- 1:n
  sum(x^2)
}
# Report the value of the sum when n=10
compute_s_n(10)
#-------------------------------------------------------------------------------
#For loops continued...
#Now we are going to compute the sum of the squares for several values of n. 
#We will use a for-loop for this. Here is an example of a for-loop:
  
  results <- vector("numeric", 10)
n <- 10
for(i in 1:n){
  x <- 1:i
  results[i] <- sum(x)
}
#Note that we start with a call to vector which constructs an empty vector that we will fill while the loop runs.

#Instructions
#Define an empty numeric vector s_n of size 25 using s_n <- vector("numeric", 25).
#Compute the the sum when n is equal to each integer from 1 to 25 using the 
#function we defined in the previous exercise: compute_s_n
#Save the results in s_n

# Define a function and store it in `compute_s_n`
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Create a vector for storing results
s_n <- vector("numeric", 25)

# write a for-loop to store the results in s_n
n<-25
for (i in 1:n){
  s_n[i]<-compute_s_n(i)
}
s_n



#-------------------------------------------------------------------------------
#Checking our math
#If we do the math, we can show that
#Sn= 1^2 + 2^2 + 3^2 +...+ n^2 = (n)(n+1)(2n+1)/6

#We have already computed the values of  from 1 to 25 using a for loop.

#If the formula is correct then a plot of  versus should look cubic.

#Let's make this plot.

#INSTRUCCIONES
#Define n <- 1:25. Note that with this we can use for(i in n)
#Use a for loop to save the sums into a vector s_n <- vector("numeric", 25)
# Plot s_n (on the y-axis) against n (on the x-axis).

# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Define the vector of n
n <- 1:25

# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}

#  Create the plot 
plot(n,s_n)
#-------------------------------------------------------------------------------
#Checking our math continued

#Instructions:
#Confirm that s_n and  (n)(n+1)(2n+1)/6 are the same using the identical command.

# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Define the vector of n
n <- 1:25

# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}

# Check that s_n is identical to the formula given in the instructions.
identical(s_n, n*(n+1)*(2*n+1)/6) 
#A:TRUE

####################################################################
####The End
####################################################################
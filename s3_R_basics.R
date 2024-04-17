###############################################################################
#indexing
###############################################################################
#Load data
library(dslabs)
data(murders)
# defining murder rate as before
murder_rate <- murders$total / murders$population * 100000
# creating a logical vector that specifies if the murder rate in that state is less 
#than or equal to 0.71. It makes a vector of 1s=True and 0s=False
index <- murder_rate <= 0.71
# determining which states have murder rates less than or equal to 0.71
murders$state[index]
# calculating how many states have a murder rate less than or equal to 0.71
sum(index)

# creating the two logical vectors representing our conditions
west <- murders$region == "West"
west
safe <- murder_rate <= 1
# defining an index and identifying states with both conditions true
index <- safe & west
murders$state[index]

###############################################################################
#indexing functions
###############################################################################

#which(), match() %in%

#The function which() gives us the entries of a logical vector that are true.
#The function match() looks for entries in a vector and returns the index needed to access them.
#We use the function %in% if we want to know whether or not each element of a first vector is in a second vector.

x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)    # returns indices that are TRUE

# to determine the murder rate in Massachusetts we may do the following
index <- which(murders$state == "Massachusetts")
index
murder_rate[index]

# to obtain the indices and subsequent murder rates of New York, Florida, Texas, we do:
index <- match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murder_rate[index]

x <- c("a", "b", "c", "d", "e")
y <- c("a", "d", "f")
y %in% x
y %in% x

# to see if Boston, Dakota, and Washington are states
c("Boston", "Dakota", "Washington") %in% murders$state

###############################################################################
#Assessment 3.1
###############################################################################
#Q1
#Which line of code will successfully filter the murders dataset to only show 
#Massachusetts? Note: Assume all state names are not capitalized in the dataset.

match(c("Massachusetts"), murders$state) #22
c("massachusetts") %in% murders$state #False


#Q2
#Which line of code will return TRUE for elements of vector ind that are missing 
#from the murders dataset?
#ind is not definded in the exercise
!ind %in% murders$state


###############################################################################
#DataCamp Assessment: 3.1 Indexing
###############################################################################

# Store the murder rate per 100,000 for each state, in `murder_rate`
murder_rate <- murders$total / murders$population * 100000

# Store the `murder_rate < 1` in `low` 
low<-murder_rate<1

# Get the indices of entries that are below 1
which(murder_rate<1)

#Note that if we want to know which entries of a vector are lower than a particular value we can use code like this:

small <- murders$population < 1000000
murders$state[small]
#The code above shows us the states with populations smaller than one million.

# Names of states with murder rates lower than 1
murders$state[which(murder_rate<1)]


# Create a vector ind for states in the Northeast and with murder rates lower than 1. 
str(murders)
low <- murder_rate < 1
Low
northeast <- murders$region == "Northeast"
northeast
ind <- low & northeast
ind
ind2<-murder_rate < 1 & murders$region == "Northeast"
northeast
ind2
# Names of states in `ind` 
murders$state[ind]

# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000


# Compute the average murder rate using `mean` and store it in object named `avg`

avg<-mean(murder_rate)

# How many states have murder rates below avg ? Check using sum 

murders$state[murder_rate<avg]

# Compute the average murder rate using `mean` and store it in object named `avg`

avg<-mean(murder_rate)
avg
# How many states have murder rates below avg ? Check using sum 

sum(murder_rate<avg)


# Store the 3 abbreviations in a vector called `abbs` (remember that they are character vectors and need quotes)
abbs<-c("AK", "MI", "IA")
# Match the abbs to the murders$abb and store in ind
ind<-match(abbs, murders$abb)

# Print state names from ind
murders$state[ind]

# Store the 5 abbreviations in `abbs`. (remember that they are character vectors)
abbs<- c("MA", "ME", "MI", "MO", "MU")

# Use the %in% command to check if the entries of abbs are abbreviations in the the murders data frame
abbs %in% murders$abb

# Store the 5 abbreviations in abbs. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU") 

# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in `ind`
ind<-which(!abbs %in% murders$abb)
ind
# Names of abbreviations in `ind`
abbs[ind]

###############################################################################
#Basic Data Wrangling
###############################################################################

##dplyr is to work with data tables. It has functions like filter, select, %>%, mutate

# installing and loading the dplyr package
install.packages("dplyr")
library(dplyr)

# adding a column with mutate
library(dslabs)
data("murders")
murders <- mutate(murders, rate = total / population * 100000)

# subsetting with filter
filter(murders, rate <= 0.71)

# selecting columns with select
new_table <- select(murders, state, region, rate)

# using the pipe
murders %>% select(state, region, rate) %>% filter(rate <= 0.71)

# creating a data frame with stringAsFactors = FALSE
grades <- data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                     exam_1 = c(95, 80, 90, 85), 
                     exam_2 = c(90, 85, 85, 90),
                     stringsAsFactors = FALSE)

###############################################################################
#DataCamp Assessment: 3.2 Basic Data Wrangling
###############################################################################

#You can add columns using the dplyr function mutate. This function is aware of
#the column names and inside the function you can call them unquoted. Like this
murders <- mutate(murders, population_in_millions = population / 10^6)

#Note that we can write population rather than murders$population. 
#The function mutate knows we are grabing columns from murders.

# Loading data
library(dslabs)
data(murders)

# Loading dplyr
library(dplyr)

# Redefine murders so that it includes a column named rate with the per 100,000 murder rates
murders <- mutate(murders, rate=total/population*100000)

#Note that if rank(x) gives you the ranks of x from lowest to highest, rank(-x) 
#gives you the ranks from highest to lowest.

# Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks 
x <- c(88, 100, 83, 92, 94)
rank(-x)

# Defining rate
rate <-  murders$total/ murders$population * 100000

# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders<-mutate(murders, rank=rank(-rate))
# Load dplyr
library(dplyr)

# Use select to only show state names and abbreviations from murders
select(murders, state, abb)

# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))

# Filter to show the top 5 states with the highest murder rates
filter(murders, rank<=5)

# Use filter to create a new data frame no_south
no_south<-filter(murders, region!= "South")
# Use nrow() to calculate the number of rows
nrow(no_south)
# Create a new data frame called murders_nw with only the states from the northeast and the west

murders_nw<-filter(murders, region %in% c("Northeast", "West"))

# Number of states (rows) in this category 
nrow(murders_nw)

# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# Create a table, call it my_states, that satisfies both the conditions 
my_states <-filter(murders, rate < 1 & region %in% c("Northeast", "West"))
# Use select to show only the state name, the murder rate and the rank

select(my_states, state, rate, rank)

#The pipe %>% can be used to perform operations sequentially without having to define intermediate objects. After redefining murder to include rate and rank.

library(dplyr)
murders <- mutate(murders, rate =  total / population * 100000, rank = (-rate))
#in the solution to the previous exercise we did the following:
  
# Created a table 
  my_states <- filter(murders, region %in% c("Northeast", "West") & rate < 1)

# Used select to show only the state name, the murder rate and the rank
select(my_states, state, rate, rank)
#The pipe %>% permits us to perform both operation sequentially and without having to define an intermediate variable my_states
#For example we could have mutated and selected in the same line like this:
  
mutate(murders, rate =  total / population * 100000, rank = (-rate)) %>%  select(state, rate, rank)

#Note that select no longer has a data frame as the first argument. The first argument is assumed to be the result of the operation conducted right before the %>%

# Load library
library(dplyr)

## Define the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# show the result and only include the state, rate, and rank columns, all in one line, in that order
filter(murders, region %in% c("Northeast", "West") & rate<1) %>% select(state,rate,rank)

# Loading the libraries
library(dplyr)
library(dslabs)
data(murders)

# Create new data frame called my_states (with specifications in the instructions)
#Use one line of code to create a new data frame, called my_states, that has murder rate and rank columns (with the rank ordered from highest to lowest), considers only states in the Northeast or West which have a murder rate lower than 1, and contain only the state, rate, and rank columns. The line should have four components separated by three %>% operators:
  
#The original dataset murders
#A call to mutate to add the murder rate and the rank.
#A call to filter to keep only the states from the Northeast or West and that have a murder rate below 1.
#A call to select that keeps only the columns with the state name, the murder rate, and the rank.
#The line should look something like this my_states <- murders %>% mutate something %>% filter something %>% select something. Columns in the final data frame MUST be in the order: state, rate, rank.

my_states <- murders %>% 
  mutate(rate =  total / population * 100000, rank = rank(-rate)) %>%
  filter(region %in% c("Northeast", "West") & rate < 1) %>%
  select(state, rate, rank)
my_states

###############################################################################
#Basic Plots
###############################################################################

library(dplyr)
library(dslabs)
data("murders")

# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)

# a histogram of murder rates
murders <- mutate(murders, rate = total / population * 100000)
hist(murders$rate)

# boxplots of murder rates by region
boxplot(rate~region, data = murders)

# Load the datasets and define some variables
library(dslabs)
data(murders)

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_in_millions, total_gun_murders)

# Transform population (not population in millions) using the log10 transformation and save to object log10_population
log10_population <- log10(murders$population)
# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
log10_total_gun_murders<-log10(murders$total)
# Create a scatterplot with the log scale transformed population and murders 

plot(log10_population, log10_total_gun_murders)

# Store the population in millions and save to population_in_millions 
population_in_millions <- murders$population/10^6


# Create a histogram of this variable
hist(population_in_millions)

# Create a boxplot of state populations by region for the murders dataset

boxplot(population~region, murders)

###############################################################################
#The summarize function
###############################################################################
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# minimum, median, and maximum murder rate for the states in the West region
s <- murders %>% 
  filter(region == "West") %>%
  summarize(minimum = min(rate), 
            median = median(rate), 
            maximum = max(rate))
s

# accessing the components with the accessor $
s$median
s$maximum

# average rate unadjusted by population size
mean(murders$rate)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

###############################################################################
#Summarizing with more than one value
###############################################################################

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# minimum, median, and maximum murder rate for the states in the West region using quantile
# note that this returns a vector
murders %>% 
  filter(region == "West") %>%
  summarize(range = quantile(rate, c(0, 0.5, 1)))

# returning minimum, median, and maximum as a data frame
my_quantile <- function(x){
  r <-  quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], median = r[2], maximum = r[3]) 
}
murders %>% 
  filter(region == "West") %>%
  summarize(my_quantile(rate))

###############################################################################
#Pull to access columns
###############################################################################

#WITH SUMMARIZE YOU GET A DATAFRAME BUT WITH PULL YOU CAN MAKE IT NUMERIC
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# us_murder_rate is stored as a data frame
class(us_murder_rate)

# the pull function can return it as a numeric value
us_murder_rate %>% pull(rate)

# using pull to save the number directly
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  pull(rate)
us_murder_rate

# us_murder_rate is now stored as a number
class(us_murder_rate)

###############################################################################
#The dot placeholder
###############################################################################

#USING THE DOT IMMITATES THE PULL FUNCTION

library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# average rate adjusted by population size
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5)
us_murder_rate

# using the dot to access the rate
us_murder_rate <- murders %>% 
  summarize(rate = sum(total) / sum(population) * 10^5) %>%
  .$rate
us_murder_rate
class(us_murder_rate)

###############################################################################
#Group then summarize
###############################################################################
#GROUO_BY() gives you a set of tables on which summarize acts differently
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# group by region
murders %>% group_by(region)

# summarize after grouping
murders %>% 
  group_by(region) %>%
  summarize(median = median(rate))

###############################################################################
#Sorting data tables
###############################################################################
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)
murders <- mutate(murders, rate = total / population * 10^5)

# order the states by population size
murders %>% arrange(population) %>% head()

# order the states by murder rate - the default is ascending order
murders %>% arrange(rate) %>% head()

# order the states by murder rate in descending order
murders %>% arrange(desc(rate)) %>% head()

# order the states by region and then by murder rate within region
murders %>% arrange(region, rate) %>% head()

# return the top 10 states by murder rate
murders %>% top_n(10, rate)

# return the top 10 states ranked by murder rate, sorted by murder rate
murders %>% arrange(desc(rate)) %>% top_n(10)

###############################################################################
#Assessment: summarizing with dplyr
###############################################################################

#To practice our dplyr skills we will be working with data from the survey collected 
#by the United States National Center for Health Statistics (NCHS). This center 
#has conducted a series of health and nutrition surveys since the 1960’s.

#Starting in 1999, about 5,000 individuals of all ages have been interviewed every 
#year and then they complete the health examination component of the survey. 
#Part of this dataset is made available via the NHANES package which can be 
#loaded this way:
install.packages(NHANES)
library(NHANES)
data(NHANES)
#The NHANES data has many missing values. Remember that the main summarization 
#function in R will return NA if any of the entries of the input vector is an NA. 
#Here is an example:
library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)
#To ignore the NAs, we can use the na.rm argument:
mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)

#######Exercise 1. Blood pressure 1
#Let's explore the NHANES data. We will be exploring blood pressure in this dataset.

#First let's select a group to set the standard. We will use 20-29 
#year old females. Note that the category is coded with 20-29, with
#a space in front of the 20! The AgeDecade is a categorical 
#variable with these ages.

#To know if someone is female, you can look at the Gender variable.

#INSTRUCTIONS
#Filter the NHANES dataset so that only 20-29 year old females are included and 
#assign this new data frame to the object tab.
#Use the pipe to apply the function filter, with the appropriate logicals, to NHANES.
#Remember that this age group is coded with 20-29, which includes a space. 
#You can use head to explore the NHANES table to construct the correct call to filter.
install.packages("NHANES")
library(dplyr)
library(NHANES)
data(NHANES)
## fill in what is needed
head(NHANES)
tab <- NHANES %>% filter(AgeDecade == " 20-29", Gender =="female")
head(tab)


-----------------------------------------------------------------------------------------
#######Exercise 2. Blood pressure 2
#Now we will compute the average and standard deviation for the subgroup we 
#defined in the previous exercise (20-29 year old females), which we will use 
#reference for what is typical.

#You will determine the average and standard deviation of systolic blood 
#pressure, which are stored in the BPSysAve variable in the NHANES dataset.

#INSTRUCTIONS
#Complete the line of code to save the average and standard deviation of systolic
#blood pressure as average and standard_deviation to a variable called ref.

#Use the summarize function after filtering for 20-29 year old females and connect 
#the results using the pipe %>%. When doing this remember there are NAs in the data!

library(dplyr)
library(NHANES)
data(NHANES)
## complete this line of code.
NHANES["BPSysAve"]
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviatio = sd(BPSysAve, na.rm = TRUE))
ref
  
-----------------------------------------------------------------------------------------
#######Exercise 3. Summarizing averages

#Now we will repeat the exercise and generate only the average blood pressure 
#for 20-29 year old females. For this exercise, you should review how to use 
#the place holder . in dplyr or the pull function.

library(dplyr)
library(NHANES)
data(NHANES)
## modify the code we wrote for previous exercise.
ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>% pull(average)

#######Exercise 4. Min and max

#Let's continue practicing by calculating two other data summaries: the minimum and the maximum.
#Again we will do it for the BPSysAve variable and the group of 20-29 year old females.
#Report the min and max values for the same group as in the previous exercises.
#Use filter and summarize connected by the pipe %>% again. The functions min and max can be used to get the values you want.
#Within summarize, save the min and max of systolic blood pressure as minbp and maxbp.

library(dplyr)
library(NHANES)
data(NHANES)
## complete the line
NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>% summarize(minbp = min (BPSysAve,na.rm=TRUE), maxbp = max(BPSysAve,na.rm=TRUE))
-----------------------------------------------------------------------------------------
#######Exercise 5. group_by

#What we are about to do is a very common operation in data science: you will split a data table into groups and then compute summary statistics for each group.

#We will compute the average and standard deviation of systolic blood pressure for females for each age group separately. Remember that the age groups are contained in AgeDecade.

#INSTRUCTIONS:

#Use the functions filter, group_by, summarize, and the pipe %>% to compute the average and standard deviation of systolic blood pressure for females for each age group separately.
#Within summarize, save the average and standard deviation of systolic blood pressure (BPSysAve) as average and standard_deviation.
#Note: ignore warnings about implicit NAs. This warning will not prevent your code from running or being graded correctly.

library(dplyr)
library(NHANES)
data(NHANES)
##complete the line with group_by and summarize
NHANES %>%
  filter(Gender == "female") %>% group_by(AgeDecade) %>% summarize(average = mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))

#######Exercise 6. group_by example 2

#Now let's practice using group_by some more. We are going to repeat the previous exercise of calculating the average and standard deviation of systolic blood pressure, but for males instead of females.

#INSTRUCTIONS

#Calculate the average and standard deviation of systolic blood pressure for males for each age group separately using the same methods as in the previous exercise.
#Note: ignore warnings about implicit NAs. This warning will not prevent your code from running or being graded correctly.

library(dplyr)
library(NHANES)
data(NHANES)
#str(NHANES)
NHANES %>% filter(Gender == "male") %>% 
  group_by(AgeDecade) %>% 
  summarize(average=mean(BPSysAve, na.rm=TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE)) 

-----------------------------------------------------------------------------------------
#######Exercise 7. group_by example 3

  
  
#We can actually combine both of these summaries into a single line of code. This is because group_by permits us to group by more than one variable.
  
#We can use group_by(AgeDecade, Gender) to group by both age decades and gender.

#Create a single summary table for the average and standard deviation of systolic blood pressure using group_by(AgeDecade, Gender).
#Note that we no longer have to filter!
#Your code within summarize should remain the same as in the previous exercises.

library(NHANES)
data(NHANES)

NHANES %>% group_by(AgeDecade, Gender) %>% summarize(average=mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve, na.rm=TRUE))

-----------------------------------------------------------------------------------------
#######Exercise 8. Arrange

  
#Now we are going to explore differences in systolic blood pressure across races, as reported in the Race1 variable.
  
#We will learn to use the arrange function to order the outcome acording to one variable.

#Note that this function can be used to order any table by a given outcome. Here is an example that arranges by systolic blood pressure.

  NHANES %>% arrange(BPSysAve)
#If we want it in descending order we can use the desc function like this:
  
  NHANES %>% arrange(desc(BPSysAve))
#In this example, we will compare systolic blood pressure across values of the Race1 variable for males between the ages of 40-49.

#INSTRUCTIONS
#Compute the average and standard deviation for each value of Race1 for males in the age decade 40-49.
#Order the resulting table from lowest to highest average systolic blood pressure.
#Use the functions filter, group_by, summarize, arrange, and the pipe %>% to do this in one line of code.
#Within summarize, save the average and standard deviation of systolic blood pressure as average and standard_deviation.
  
  library(dplyr)
  library(NHANES)
  data(NHANES)
  str(NHANES)
  NHANES %>% filter(AgeDecade ==" 40-49" & Gender == "male") %>% group_by(Race1) %>% summarize(average=mean(BPSysAve, na.rm=TRUE), standard_deviation=sd(BPSysAve, na.rm=TRUE)) %>% arrange(average)
  
  

  
###############################################################################
#Introduction to data.table
###############################################################################

  
#In this course, we often use tidyverse packages to illustrate because these packages tend to have code that is very readable for beginners.
  #There are other approaches to wrangling and analyzing data in R that are faster and better at handling large objects, such as the data.table package.
  #Selecting in data.table uses notation similar to that used with matrices.
  #To add a column in data.table, you can use the := function.
  #Because the data.table package is designed to avoid wasting memory, when you make a copy of a table, it does not create a new object. The := function changes by reference. If you want to make an actual copy, you need to use the copy() function.
  #Side note: the R language has a new, built-in pipe operator as of version 4.1: |>. This works similarly to the pipe %>% you are already familiar with.

  
  # install the data.table package before you use it!
  install.packages("data.table")
  
  # load data.table package
  library(data.table)
  
  # load other packages and datasets
  library(tidyverse)
  library(dplyr)
  library(dslabs)
  data(murders)
  
  # convert the data frame into a data.table object
  murders <- setDT(murders)
  
  # selecting in dplyr
  select(murders, state, region)
  
  # selecting in data.table - 2 methods
  murders[, c("state", "region")] |> head()
  murders[, .(state, region)] |> head()
  
  # adding or changing a column in dplyr
  murders <- mutate(murders, rate = total / population * 10^5)
  
  # adding or changing a column in data.table
  murders[, rate := total / population * 100000]
  head(murders)
  murders[, ":="(rate = total / population * 100000, rank = rank(population))]
  
  # y is referring to x and := changes by reference
  x <- data.table(a = 1)
  y <- x
  
  x[,a := 2]
  y
  
  y[,a := 1]
  x
  
  # use copy to make an actual copy
  x <- data.table(a = 1)
  y <- copy(x)
  x[,a := 2]
  y

  
###############################################################################
# Subsetting with data.table
###############################################################################
  
  # load packages and prepare the data
  library(tidyverse)
  library(dplyr)
  library(dslabs)
  data(murders)
  library(data.table)
  murders <- setDT(murders)
  murders <- mutate(murders, rate = total / population * 10^5)
  murders[, rate := total / population * 100000]
  
  # subsetting in dplyr
  filter(murders, rate <= 0.7)
  
  # subsetting in data.table
  murders[rate <= 0.7]
  
  # combining filter and select in data.table
  murders[rate <= 0.7, .(state, rate)]
  
  # combining filter and select in dplyr
  murders %>% filter(rate <= 0.7) %>% select(state, rate)  
 
  
###############################################################################
# Subsetting with data.table
##Summarizing with data.table##
###############################################################################
  # load packages and prepare the data - heights dataset
  library(tidyverse)
  library(dplyr)
  library(dslabs)
  data(heights)
  heights <- setDT(heights)
  
  # summarizing in dplyr
  s <- heights %>% 
    summarize(average = mean(height), standard_deviation = sd(height))
  
  # summarizing in data.table
  s <- heights[, .(average = mean(height), standard_deviation = sd(height))]
  
  # subsetting and then summarizing in dplyr
  s <- heights %>% 
    filter(sex == "Female") %>%
    summarize(average = mean(height), standard_deviation = sd(height))
  
  # subsetting and then summarizing in data.table
  s <- heights[sex == "Female", .(average = mean(height), standard_deviation = sd(height))]
  s
  # previously defined function
  median_min_max <- function(x){
    qs <- quantile(x, c(0.5, 0, 1))
    data.frame(median = qs[1], minimum = qs[2], maximum = qs[3])
  }
  
  # multiple summaries in data.table
  heights[, .(median_min_max(height))]
  
  # grouping then summarizing in data.table
  heights[, .(average = mean(height), standard_deviation = sd(height)), by = sex]

###############################################################################
#  Sorting data frames 
###############################################################################
  
  # load packages and datasets and prepare the data
  library(tidyverse)
  library(dplyr)
  library(data.table)
  library(dslabs)
  data(murders)
  murders <- setDT(murders)
  murders[, rate := total / population * 100000]
  
  # order by population
  murders[order(population)] |> head()
  
  # order by population in descending order
  murders[order(population, decreasing = TRUE)] 
  
  # order by region and then murder rate
  murders[order(region, rate)]
  
###############################################################################
# TIBBLES
###############################################################################
  
#TIBBLES are the default with tydiverse
  #Tibbles display better than regular data frames.
  #Subsets of tibbles are tibbles, which is useful because tidyverse functions require data frames as inputs.
  #Tibbles will warn you if you try to access a column that doesn't exist.
  #Entries in tibbles can be complex - they can be lists or functions.
 #The function group_by() returns a grouped tibble, which is a special kind of tibble.
  
  # view the dataset
  murders %>% group_by(region)
  
  # see the class
  murders %>% group_by(region) %>% class()
  
  # compare the print output of a regular data frame to a tibble
  gapminder
  as_tibble(gapminder)
  
  # compare subsetting a regular data frame and a tibble
  class(murders[,1])
  class(as_tibble(murders)[,1])
  
  # access a column vector not as a tibble using $
  class(as_tibble(murders)$state)
  
  # compare what happens when accessing a column that doesn't exist in a regular data frame to in a tibble
  murders$State
  as_tibble(murders)$State
  
  # create a tibble
  tibble(id = c(1, 2, 3), func = c(mean, median, sd))
      
###############################################################################
  # Section 3 Assessment
###############################################################################
  library(dslabs)
  data(heights)
  options(digits = 3)    # report 3 significant digits for all answers
  
#________________________________________________________________________________
  #Question 1
  #First, determine the average height in this dataset. Then create a logical 
  #vector ind with the indices for those individuals who are above average height.
  #How many individuals in the dataset are above average height?
  str(heights)
  avg <-mean(heights$height)
  avg
  ind <-which(heights$height > avg)
  ind
  #heights$height[ind]
  length(ind) #532
  
  #answer2
  library(data.table)
  heightss <- setDT(heights)
  which(as_tibble(heightss)$height>mean(heightss$height)) %>% length() #532
  
  
#________________________________________________________________________________
###Question 2
  
#How many individuals in the dataset are above average height and are female?

plus_avg_fem<- heights %>% filter(sex == "Female") %>% select(height)
plus_avg_fem
which(plus_avg_fem> mean(heights$height)) %>% length() #31

#answer 2
avg <-mean(heights$height)
plus_avg_fem2<-heights[sex == "Female", height>avg] %>% which(TRUE) %>% length() #31
plus_avg_fem2

#________________________________________________________________________________
###Question 3
#What proportion of individuals in the dataset are female?

fem_tot<-heights %>% filter(sex =="Female") %>% pull(height)
class(fem_tot)
length(fem_tot) #238
length(heights$height) #1050
length(fem_tot)/length(heights$height)*100 #22.7 ->report as 0.227


###also:
fem_tot2<-heights[sex =="Female"] %>% pull(height) %>%length()
fem_tot2

#________________________________________________________________________________
###Question 4
##Question 4a
#Determine the minimum height in the heights dataset.
heights %>% summarize(minh=min(height, na.rm=TRUE)) #50
#Answer 2
library(data.table)
heightss <- setDT(heights)
heightss[, .(min(height))] # 50 - with  library(data.table)

##Question 4b
#Use the match() function to determine the index of the first individual with the minimum height.
min_match<-match(min(heights$height), heights$height) #1032


##Question 4c
#Subset the sex column of the dataset by the index in 4b to determine the individual’s sex.
heights$sex[min_match] #Male
#________________________________________________________________________________
###Question 5
#This question takes you through three steps to determine how many of the integer 
#height values between the minimum and maximum heights are not actual heights of 
#individuals in the heights dataset.

###Question 5a
#Determine the maximum height.
max(heights$height) #82.7


#Question 5b
#Which integer values are between the maximum and minimum heights? For example, 
#if the minimum height is 10.2 and the maximum height is 20.8, your answer 
#should be x <- 11:20 to capture the integers in between those values. 
#(If either the maximum or minimum height are integers, include those values too.)

#Write code to create a vector x that includes the integers between the minimum 
#and maximum heights in this dataset (as numbers).
#There are multiple ways to solve this problem, but the grader expects you to use 
#the format in the problem description. Your answer should have numbers and a colon (:), and it should not use other functions.
min(heights$height)
max(heights$height)
x<-min(heights$height):max(heights$height)
#x<-50:82

#Question 5c
#How many of the integers in x are NOT heights in the dataset?
sum(!x %in% heights$height) #3

#________________________________________________________________________________
###Question 6

#Using the heights dataset, create a new column of heights in centimeters named 
#ht_cm. Recall that 1 inch = 2.54 centimeters. Save the resulting dataset as 
#heights2.
heights2 <- mutate(heights, ht_cm = height * 2.54)

#Question 6a
#What is the height in centimeters of the 18th individual (index 18)?
heights2$ht_cm[18] #163

#Question 6b
mean(heights2$ht_cm) #174

#________________________________________________________________________________
###Question 7
#Create a data frame females by filtering the heights2 data to contain only female individuals.
fem_ht<-heights2[sex=="Female"]
class(fem_ht)

fem_ht2<-heights2 %>% filter(sex=="Female")
class(fem_ht2)

#Question 7a
#How many females are in the heights2 dataset?
length(fem_ht$height) #238
length(fem_ht2$height) #238

#Question 7b
#What is the mean height of the females in centimeters?
mean(fem_ht$ht_cm) #165
#________________________________________________________________________________
###Question 8

library(dslabs)
data(olive)
head(olive)
as_tibble(olive)

plot(olive$palmitic, olive$palmitoleic)
#positive relationship between palmitic and palmitoleic
#________________________________________________________________________________
###Question 9
#Create a histogram of the percentage of eicosenoic acid in olive.
hist(olive$eicosenoic) #The most common value of eicosenoic acid is below 0.05%

#________________________________________________________________________________
###Question 10
#Make a boxplot of palmitic acid percentage in olive with separate distributions for each region.

boxplot(palmitic~region, data = olive)

#Which region has the highest median palmitic acid percentage?
#A: Southern Italy

#Which region has the most variable palmitic acid percentage?  
#A: Southern Italy


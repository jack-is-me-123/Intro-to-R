# Workshop 4
# Writing Functions and Iteration
# Author: Jack Marks
# July 2020

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!require("readxl")) install.packages("readxl")
library("readxl")

if (!require("readxl")) install.packages("readxl")
library("readxl")

if (!require("checkmate")) install.packages("checkmate")
library("checkmate")

if (!require("readr")) install.packages("readr")
library("readr")

if (!require("testthat")) install.packages("testthat")
library("testthat")

# Introduction ----

# Throughout this course, we have been working with functions which are 
# written in different packages. One of the great things about R is how easy
# it is to write your own functions that you can use repeatedly within
# one project, or across your work. Writing functions can seem daunting at
# first, but it's as simple as this: almost anything that you want R to do, that
# R takes more than one step to do, can be written as a function that lets you
# do it in only one step.

# Writing your own functions lets you wrap up processes that you repeat
# throughout your work in a neat way, letting you write neater and shorter 
# scripts. These functions can also be made to be flexible, so that they can be 
# used in other settings which you might be useful. Further, they can be made
# robust by adding in tests that make sure specific criteria are met by inputs
# and outputs.

# Let's begin with a simple example. Let's say we want to take the sum of two 
# numbers, and the product of two numbers, and add them together.We would want
# something like this: (x + y) + (x * y)
# We can do that in R by explicitly setting out the equation in its entirity.

x <- 17
y <- 23

(x + y) + (x * y)


# This will give us our answer, but if this was a process we did repeatedly
# throughout our work, on different data inputs, it would be more convenient
# to be able to do this as a single function. 

# To create a function, we begin the same way we do when creating any other
# object in R - we use the assignment operator (<-) to tell R we intend to
# create something new for R to save. we'll call this function "do_a_thing"

do_a_thing <-

# We then write function() with  a list of arguments that we want the function 
# to take. we want a function that will do some arithmetic to two numbers, so 
# our function will have two arguments. We'll call these arguments "x" and "y".

do_a_thing <- function(x, y)
  
# We then open up some curly brackets, where we'll tell R what we want R to do
# to the arguments we've set out (note that we put the end bracket on its own
# line to make it easier to read - this is a common convention for writing R
# and one that should not be broken)

do_a_thing <- function(x, y) {
  (x + y) + (x * y)
} 

# If we run this, we can see that do_a_thing() is now in our global
# environment listed as a function. We can use it by assigning values to go into
# it.

do_a_thing(x = 17, y = 23)
do_a_thing(x = 2.45, y = 394.85)



# Challenge:
# write a function that takes the sum of three values, and squares them.




# What to name your functions ----

# There are different style guides one can follow when naming functions (or
# doing anything in R). The most important thing is that your function name 
# provides as clear an idea as possible of what your function does.
# The tidyverse style guide (which I like to use) also recommends that your
# function be a verb, or a verb + object. For example, we might name our
# do_a_thing function something like "add_and_add_squares". 


# An example with data ----

# To see how a function can be applied to a data set, let's use some data.

exams <- read_csv("data/exams.csv")

head(exams)

# This data set is a sample data set of student's scores in 3 subjects. 
# It also gives us some information on the student's parent's highest level of
# education, whether the student did a prep course for the tests, their gender,
# and whether they receive free school lunch (dataset is  randomly generated).

# Let's imagine we are trying to determine if any of the students in this group
# should be flagged as potentially having a reading difficulty We might expect
# that such a student could find math much easier than history or english (two 
# rather "wordy" subjects).

# We can write a function that will examine whether a student's math score is 
# more than 50% higher than their score in english and history.

test_reading <- function(math_score, english_score, history_score) {
  if_else(math_score >= 1.5*english_score & 
              math_score >= 1.5* history_score, "FLAG", "no flag")
}

# Let's demonstrate this at work with the function used by itself.

test_reading(math_score = 88, english_score = 50, history_score = 43)

# We can run the function on our whole dataset, and save the output as a new
# variable in our dataset with the mutate function

exams <- exams %>%
  mutate(reading_difficulty = test_reading(math_score = math, 
                                     english_score = english,
                                     history_score = history))

head(exams)



# Unit Testing ----

# Unit testing is the process of writing tests into your code to make sure 
# inputs and outputs are in expected formats and that your code is operating as 
# expected. Unit testing can help make code more robust to changes in inputs or
# to changes in package versions in R, and helps make it easier to identify
# where errors are happening. It can also make it easier for future users (or 
# future you) to identify where a piece of code is falling-down. It's similar
# to "assert" statements in Stata code.

# There are a couple of different ways of building in unit testing: we can use
# base R if/else statements, or use functions such as stopifnot(), or use 
# functions provided by packages built for unit testing such as checkmate and
# testthat.

# Let's look at a simple example - editing our "do_a_thing" function to write 
# tests to make sure the data is in an expected format. Here, we're going to 
# tell R to expect our two data inputs to be of a numeric data type.

do_a_thing_2 <- function(x, y) {
  checkmate::expect_numeric(x)
  checkmate::expect_numeric(y)
  (x + y) + (x * y)
} 

# This can prevent our function from trying to run data that it shouldn't try
# to run, giving us an error message instead. Let's compare by running our
# original function and our new function (with checks).

do_a_thing(x= TRUE, y = 17)

do_a_thing_2(x = TRUE, y = 17)

# We can see that our first function runs on the Boolean value of TRUE. This is
# because, under the hood, R stores Boolean values as 0 and 1 for TRUE and FALSE.
# Similar errors could occur if we had variables in other data formats. Writing
# tests like this could help future you identify where data processes have gone
# wrong, and stop people using your code from assuming it has run as expected.

# Depending on the reliability of the inputs you use in your work, you may need
# to consider having a range of tests built in to your code if you want to make 
# it robust to these sorts of changes.

# Let's look at another way we might use functions and unit testing to reliably
# read in data that comes in a predictable format.

# Reading in Data ----

# Recall in the last workshop on Tidy Data we read in some untidy data from 
# Excel and turned it into a tidy data frame. Now let's assume that this data
# comes in in the same format every month, and we want to write a function for 
# bringing this data in. Our original code (when taking out all of the comments)
# looked like this.

df_untidy <- read_excel("data/Model_Output.xlsx",col_names = FALSE)
df_name <- pull(df_untidy[1,1])
df_untidy <- df_untidy[-c(1, 2, 3, 4, 6) , ]
df_untidy <- df_untidy[ , -c(1, 2)]
df_untidy[1,1] <- "id"
df_untidy <- df_untidy %>% janitor::row_to_names(row_number = 1)
df_untidy <- df_untidy %>% janitor::clean_names(case = "upper_camel")
df_untidy <- df_untidy %>% filter(!is.na(Region) & !is.na(StateSpend))
df_untidy$Date <- as.numeric(df_untidy$Date)
df_untidy$Date <-janitor::excel_numeric_to_date(df_untidy$Date,
                                                date_system = "modern",
                                                include_time = "FALSE")

# First, we can RAP all of this into a single function. Let's call our function
# "read_output". This function will only need one argument: the path to an excel
# Model_Output file.

# Note here that we explicitly specify what the function has to "return", as
# we're making a series of edits to an object called "df" within the function. 
# Sometimes you might write complex functions that involve the creation of
# multiple objects, so you will have to use the 'return()' function at the end
# of the function to specify which object you want returned. This also makes it
# easier for people QAing the code to see exactly what a function is making.

read_output <- function(xl_path) {
  df <- read_excel(xl_path,col_names = FALSE)
  df <- df[-c(1, 2, 3, 4, 6) , ]
  df <- df[ , -c(1, 2)]
  df[1,1] <- "id"
  df <- df %>% janitor::row_to_names(row_number = 1)
  df <- df %>% janitor::clean_names(case = "upper_camel")
  df <- df %>% filter(!is.na(Region) & !is.na(StateSpend))
  df$Date <- as.numeric(df$Date)
  df$Date <-janitor::excel_numeric_to_date(df$Date,
                                                  date_system = "modern",
                                                  include_time = "FALSE")
  return(df)
}

# Now let's try to use our new function.
Model_Output_1 <- read_output(xl_path = "data/Model_Output.xlsx")

head(Model_Output_1)

# While this function is useful in and of itself, it would be more robust if we
# could make sure that it would give error messages if something went wrong.
# Let's start by making sure that the function will give us an error if we don't
# have a value in cell [1, 1]. We can do this by using the function is.na() to
# check if the cell has a missing value (we will put a '!' in front of it to 
# specify we want to check if it is NOT NA), and use the pull() function so we
# just pull out the value (rather than confusing R with meta information about 
# the data type of the data frame)

read_output <- function(xl_path) {
  df <- read_excel(xl_path, col_names = FALSE) # note adding this in to explicitly make blank cells NAs
  stopifnot(!is.na(pull(df[1,1]))) # check that cell [1, 1] does not have an NA in it
  stopifnot(allMissing(df[2 , ]) == TRUE) # check that the second row is all NA
  df <- df[-c(1, 2, 3, 4, 6) , ]
  df <- df[ , -c(1, 2)]
  df[1,1] <- "id"
  df <- df %>% janitor::row_to_names(row_number = 1)
  df <- df %>% janitor::clean_names(case = "upper_camel")
  df <- df %>% filter(!is.na(Region) & !is.na(StateSpend))
  df$Date <- as.numeric(df$Date)
  df$Date <-janitor::excel_numeric_to_date(df$Date,
                                                  date_system = "modern",
                                                  include_time = "FALSE")
  return(df)
}

# If we run this on a version of the output that has a blank value for cell
# [1, 1] our function now stops running and tells us that it did so because
# "!is.na(pull(df[1, 1])) is not TRUE" - ie, it couldn't find a value in cell
# [1,1]

read_output(xl_path = "data/Model_Output_3.xlsx")
read_output(xl_path = "data/Model_Output.xlsx")

# We could include a number of tests to make this read_output() function more 
# robust. For example, we could make sure that it has an expected number of 
# columns after being tidied, or that the date variable has been carried into R
# as expected.

# Challenge ----
# Write some more tests into the read_output() function. Write tests to check
#   - that the column names are as expected after tidying
#         - look up the checkNames() function
#   - that the date variable is being imported as expected
#         - maybe check that the date is numeric when it's first brough across
#           (so not a character variable with months in text form), and maybe
#           check the dates fall into an expected range 
#           (eg > 01/01/2010 and < 01/01/2030)
#   - that the StateSpend and NumberOfStateSupported variables are always
#     positive numbers.


# Organising Functions ----

# For ease of reading this workshop, we've been writing all of our functions
# into our workshop script. However, this is not the best way to organise 
# functions. If other users wanted to use the functions you write in another 
# project, they would have to find where you're function was within an R 
# script, and just copy and paste it out to use it again. A more useful way of 
# organising our functions is to put functions in their own R scripts which can
# be read in by themselves to any project.

# Have a look in the course folder in the sub-folder 'functions'. Then open the 
# script "reading_flag'. We can see that what this is is just the function for
# flagging a student's reading ability by itself. We can read this into our R 
# script (and thus read the function into our global environment) by using the
# R function source() which is a useful way to read and execute the entirety of 
# an R script within another R script.

source("functions/reading_flag.R")

# This script can be pointed to from other scripts and the function read
# straight in. This prevents the need for writing the same functions all over 
# different work spaces.

# Encapsulation and Generalisation ----

# Much of making code more efficient is the process of "encapsulation" and 
# "generalisation". Encapsulation is the process of taking a series of steps
# and turning them into one step (building a function). Generalisation is about
# taking a function and generalising, increasing the number of cases in which it
# can be usefully applied. 

# Let's go back to our reading difficulty score. We can make this function more
# generalised by letting the user set ratio that triggers the flag. For example,

reading_flag_2 <- function(math_score, english_score, history_score, ratio) {
  if_else(math_score >= ratio * english_score & 
            math_score >= ratio * history_score, "FLAG", "no flag")
}

# Now the user of the function can change ratio which will trigger the flag.

exams <- exams %>%
  mutate(reading_difficulty = reading_flag_2(math_score = math, 
                                           english_score = english,
                                           history_score = history,
                                           ratio = 1.2))

exams %>% filter(reading_difficulty == "FLAG")

# Finally, we can add defaults to our arguments that make our function easier 
# to use at first instance, but still keep the same flexibility. For example,
# we might set a default level for the ratio for the reading flag to trigger of
# 1.5. Users can then leave this argument blank and use the default setting of 
# the ratio argument.

reading_flag_3 <- function(math_score, english_score, history_score, 
                           ratio = 1.5) {
  if_else(math_score >= ratio * english_score & 
            math_score >= ratio * history_score, "FLAG", "no flag")
}

# When you write your own functions, you should think about how you can increase
# the flexibility of them so they can be useful to you and your team members in
# more circumstances.

# Iteration ----

# If you want to be able to  write efficient, readable code, you will need to be
# able to write code that can iterate - ie code that can repeat across a series
# of values or objects. Code becomes harder to QA, harder to edit, and more
# prone to error when similar blocks of code are written repeatedly.

# There are two commonly used ways of iterating code in R: 
#   - for loops
#   - apply functions.

# for loops ----

# for loops are constructed by specifying a series of values you want an
# operation to be repeated over, and the operation you want done. Let's look at 
# a simple for loop

for(i in 1:10){ # for i (every value) between 1 and 10
  print(i * 2) # print the product of that value multiplied by 2
}

# We can edit this for loop to read in an input and produce an output.

# define the vector
some_numbers <- 1:10

# define an (empty) output
more_numbers <- double()

# use for loop to double the numbers
for(i in some_numbers){
  more_numbers[i] <- i * 2
}

# next, let's add some dependency to this for loop, for example only
# multiplying numbers that are divisible by 2. We can do this with the if() and 
# else() functions nested within the for loop

# define an (empty) output
more_numbers <- double()

# use for loop to double the numbers
for(i in some_numbers){
  if(i %% 2){ # if value is divisible by 2 (with no remainder)
    more_numbers[i] <- i * 2 # double the value
  } else { # if that condition not met
  more_numbers[i] <- i # take original value
  }
}


# for loops (and related functions such as while(), if(), and else() ) are very 
# powerful ways to achieve a lot of computation in a small amount of code. This
# makes code that is more efficient to write, more efficient to read, and easier
# to edit in future. 

# Challenge ----

# Start by writing a for loop that takes some_numbers and squares them, printing
# the results




# edit the for loop so that the output is saved as an object called
# "squared_numbers"



# Finally, build in a conditional test, where the number is only squared if it's
# an even number. If it's an odd number, multiply it by 3.





# apply functions ----

# apply functions are an alternative to for loops, which can be used for 
# iterating a function or a set of functions over multiple objects - for example 
# all elements in a list. 

# Let's demonstrate this by using some apply functions to explore the exams
# data set above.

# When R holds data as a data.frame, R is actually holding it in memory as a 
# list of vectors, where each vector is a variable. We can therefore use lapply()
# to apply the same function to all the variables, as lapply() is designed to 
# iterate a function over a list

lapply(exams, mean) # this reads as "take the exams data set, and apply the 
                    # mean() function to all variables

# A useful feature of apply functions is that they can work within or at the end
# of piped functions. For example, we can select out only the variables we 
# want the mean of in a pipe before using lapply

exams %>% 
  select(math:english) %>%
  lapply(mean)

# Different apply functions can be used on the same input and produce different
# outputs. For example, the sapply() function takes similar input data as lapply
# but tries to return a simpler output (eg a named vector)

exams %>%
  select(math:english) %>%
  sapply(mean)

# There are a range of apply functions (lapply, sapply, apply, mapply, vapply,
# rapply, tapply). They all function in similar ways, but are designed to work
# with and output different data classes - for example, lapply is optimised for
# reading in and outputting lists.

# Challenge ----
# use an apply function to get the lowest score in the exam subjects.
# try using different apply functions - see if they work, and how their output
# differs.


# Anonymous Functions ----

# Anonymous functions are a way of increasing the flexibility of functions used 
# within apply functions. Say for example, we wanted to take the scores for 
# english, maths, and history and change every score to the distance between the
# score and the median score for that subject. We could do this by first defining
# a function (as we've learnt above), and then using that function within an 
# apply function.

translate_score <- function(x){
  median <- median(x)
  x <- x - median
}

exams %>%
  select(math:english) %>%
  sapply(translate_score)

# However, if we knew we had no intention of using this function elsewhere, we 
# could avoid adding it to our workspace (and tidy up our code a bit in the 
# process) by defining the function WITHIN the apply function. This is called an
# anonymous function as we define and use a function, without ever saving it to 
# our environment.

exams %>% 
  select(math:english) %>%
  sapply(function(x){ # we tell sapply() that we'll be applying a function with 
                      # one input
    median <- median(x) # we then define our function in the same way as before
    x <- x - median
  })

# We can also use anonymous functions to add more arguments to a function we 
# are applying within an apply function. For example, let's say we want to pull
# the 75th percentile score for each subject. We can use the quantile function,
# but the function also requires the argument for the what percentile(s) we want
# calculated. Rather than define and save an unnecessary function, we can use an
# anonymous function to define the function quantile() with the argument for 
# which percentile we want.

exams %>%
  select(math:english) %>%
  sapply(function(x){
    quantile(x, probs = 0.75)
  })

# Challenge ----

# Use an anonymous function to get the interquartile range for each test score



# apply function or for loops? ----

# There are no hard and fast rules for when for loops are preferable to apply
# functions or visa-versa. Some coders will become more comfortable with one and
# will generally lean towards it - coders who learn Python before R are usually
# happier using for loops as their application is very similar in Python, while 
# apply functions are designed to integrate well with tidyverse R principles and
# don't always have a like-for-like comparison in other coding languages.

# Some things to consider: 
# The advantage of for loops are that they typically provide more flexibility,
# particularly for advanced coders. In for loops, it is easier to implement
# conditional behaviour, where behaviour changes for different items in the list
# based on some condition, or where the outcome of one loop becomes an input
# into the next for loop. We did this above when we had the loop treate odd and
# even numbers differently. Their disadvantage comes in the fact that they can
# look more complicated for beginner coders (as they are quite different to
# tidyverse coding principles), and can take more lines of code to do the same
# thing - particularly for simpler tasks. Also, they cannot be neatly
# implemented within a tidyverse pipe (%>%) - though piped operations can be
# used within them.

# The advantage of an apply function is that it can be easier to read and write
# for less experienced coders (unless coming from another coding language which
# uses for loops more). apply functions also work with the pipe framework found
# in tidyverse, so are often easier to work in to a series of operations. Their
# disadvantage comes in that, unlike for loops, it’s difficult to make loops
# where items are treated differently depending on a condition.

# In short, for loops are better where conditional behaviour is required. Where
# the same operation is applied independently, in the same way, to all items,
# apply functions are likely more appropriate - particularly for less
# experienced coders.

# Let's demonstrate this by doing the same operation with both a for loop and 
# an apply function.

# Let's make some  dummy data - daily temperature of cities over a week
Liverpool <- c(12, 15, 16, 14,17,10, 8)
Manchester <- c( 13, 15, 17, 14, 19, 12, 10)
London <- c(15, 17, 20, 18, 22, 8, 4)

# list cities and apply names of cities
cities <- list(Liverpool, Manchester, London)
names(cities) <- c("Liverpool", "Manchester", "London")

# Get the maximum temperature for each week in each city

# apply method
max_temp <- sapply(cities, max)
max_temp

# for loop method
max_temp <- double()
for(city in names(cities)) {
  max_temp[[city]] <- max(cities[[city]])
}
max_temp

# Closing Notes ----

# This workshop has given a VERY cursory introduction to building your own 
# functions, building in some unit testing for them, and a very basic
# introduction to how to iterate code efficiently using for loops and apply 
# functions. However, this has just scratched the surface of what can be
# achieved with these coding principles. Some good workshops are recommended
# below which go into  more detail on these topics.

# Extra Material ----

# 1) A good, high-level workshop of function writing and for loops can be found
# here: https://www.earthdatascience.org/courses/earth-analytics/automate-science-workflows/write-function-r-programming/


# 2) A good workshop on functions can also be found here:
# https://workshops.rc.virginia.edu/lesson/r-functions/#fn10

# 3 ) For a look at some more advanced areas of writing functions (including
# when and how to use anonymous functions), see here: 
# http://adv-r.had.co.nz/Functional-programming.html 


# 4) You can find a good workshop on the apply family of functions here:
# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

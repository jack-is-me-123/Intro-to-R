# Workshop 4
# Writing Functions and Unit Testing
# Author: Jack Marks
# July 2020

# install packages first if need be with:
#   install.packages("tidyverse")
#   install.packages("tidyr")
#   install.packages("readr")
#   install.packages("lubridate")
#   install.packages("readxl")
#   install.packages("checkmate")

library(readxl)
library(tidyverse)
library(tidyr)
library(readr)
library(checkmate)
library(testthat)

# Introduction ----

# Throughout this course, we have been working with functions which are 
# written in different packages. One of the great things about R is how easy
# it is to write your own functions that you can use repeatedly within
# one project, or across your work. Writing functions can seem daunting at
# first, but it's as simple as this: almost anything that you want R to do, that
# R takes more than one step to do, can be written as a function that lets you
# do it in one step.

# Writing your own functions lets you wrap up processes that you repeat
# throughout your work in a neat way, letting you write neater and shorter 
# scripts. These functions can also be made to be flexible, so that they can be 
# used in other settings which you might find them useful. And they can be made
# robust, by adding in tests that make sure specific criteria are met by inputs
# and outputs..

# Let's begin with a simple example. Let's say we want to take the sum of two 
# numbers, and the product of two numbers, and add them together.We would want
# something like this: (x + y) + (x * y)
# We can do that in R by explicilty setting out the equation in its entirity.

x <- 17
y <- 23

(x + y) + (x * y)


# This will give us our answer, but if this was a process we did repeatedly
# throughout our work, on different data inputs, it would be more convenient
# to be able to do this as a single function. 

# To create a function, we begin the same way we do when creating any 
# other object in R - we use the assignment operator (<-) to tell R we intend to
# create something new for R to save. we'll call this function "do_a_thing"

do_a_thing <-

# We then write function() with  a list of arguments that we want the function 
# to take. we want a function that will do some arithmitic to two numbers, so 
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



# Challenge 1:
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
# and whether they receive free school lunch (dataset is totally 
# randomly generated).

# Let's imagine we are trying to determine if any of the students in this group
# suffer from dyslexia or some other form of reading disability. We might expect
# that such a student could find math much easier than history or english (two 
# rather "wordy" subjects).

# We can write a function that will examine whether a student's math score is 
# more than 50% higher than their score in english and history.

test_reading <- function(math_score, english_score, history_score) {
  case_when(math_score >= 1.5*english_score & 
              math_score >= 1.5* history_score ~ "FLAG",
            TRUE ~ "no flag")
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

# If we run this on a version of the output that has a blank vlaue for cell
# [1, 1] our function now stops running and tells us that it did so because
# 

read_output(xl_path = "data/Model_Output_3.xlsx")
read_output(xl_path = "data/Model_Output.xlsx")

# We could include a number of tests to make this read_output() function more 
# robust. For example, we could make sure that it has an expected number of 
# columns after being tidied, or that the date variable has been carried into R
# as expected.

# Challenge 2: 
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
# project, they would have to read find where you're function was within an R 
# script, and just copy and paste it out to use it again. A more useful way of 
# organising our functions is to put functions in their own R scripts which can
# be read in by themselves to any project.

# Have a look in the course folder in the sub-folder 'functions'. Then open the 
# script "reading_flag'. We can see that what this is is just the function for
# flagging a student's reading ability by itself. We can read this into our R 
# script (and thus read the function into our global environment) by using the
# R function source() which is a useful way to read and execute the entirity of 
# an R script within another R script.

source("functions/reading_flag.R")

# This script can be pointed to from other scripts and the function read
# straight in. This prevents the need for writing the same functions all over 
# different work spaces.

# Extra Material ----

# 1) Andreas Soteriades did a coffee and coding presentation about the use of 
# functions which you can find here: https://github.com/DataS-DHSC/coffee-and-coding/blob/master/2020-09-24 R Functions/

# 2) A good workshop on functions can also be found here:
# https://workshops.rc.virginia.edu/lesson/r-functions/#fn10

# 3 ) For a look at some more advanced areas of writing functions (including
# when and how to use anonymouse functions), see here: 
# http://adv-r.had.co.nz/Functional-programming.html 


# Addendum: Apply() functions ----
# 4 ) The apply family of functions
# Though they don't fit neatly into any of the topics covered in this course,
# some R users may find themselves encountering the apply family of functions 
# a LOT. What they let you do is apply the same function with the same arguments
# across a range of objects you have grouped together. This can be very useful, 
# for example, when you have a large number of dataframes which you want to edit
# in the same way. 
# Imagine you have a set of 12 dataframes for data from each month of the year.
# All the dataframes need the same edits before you merge them together (with a
# bind function) to do your analysis. One approach, which we explored above, is
# to write a function that contains all of the edits you would need to do to a
# data frame, and then run that function on each individual data frame. However,
# another approach is to take all the data frames, group them together in a 
# list, and then tell R to run a function on all items in that list in the same 
# way. 
# There isn't time or room enough in this course to cover every avenue that we 
# could go down that could improve R work, and the apply family of functions can
# seem very complicated and unfriendly at first, but they're worth checking out
# if you really want to develop as an R user.

# You can find a good workshop on this family o functions here:
# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family

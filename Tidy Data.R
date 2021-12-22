# Workshop 2
# Introduction to Data Tidying
# Author: Jack Marks
# July 2020

# Before you start, don't forget to set your working directory in Session ->
# Set Working Directory -> To Source File Location

# In the last workshop we learned some basics to using R and R studio, as well
# as learning about the most commonly used dplyr functions (select,
# filter, mutate, arrange, _joins, _binds, group_by and summarise).
# However, these functions are most useful when we have "tidy data".
# In this workshop, we're going to learn about tidy data: what it is, and how 
# to take "untidy data" and manipulate it into a tidy format that 
# we can use for data analysis.

# Let's load in our packages
# A reminder that tidyverse contains a suite of packages such as dplyr,
# ggplot2 (for plotting).

# The code below is a nice way of loading packages - it will only install a
# package if it's not already installed. This is useful when reusing scripts.
# It doesn't matter if someone is coming to this script with the packages 
# installed or not - if they're not installed, they will be - and if they're
# pre-installed then the time consuming installation is skipped.

if (!require("readxl")) install.packages("readxl")
library("readxl")

if (!require("tidyverse")) install.packages("tidyverse")
library("tidyverse")

if (!require("tidyr")) install.packages("tidyr")
library("tidyr")

# What is Tidy Data? ----

# There are 3 inter-related principles that make data tidy:
# - Each variable must have its own column.
# - Each observation must have its own row.
# - Each value must have its own cell.

# These principles are inter-related as it's impossible to satisfy 2 of
# these principles without satisfying all 3.

# Let's see this in practice by looking at some data.
# We're going to use some sample data included in the
# tidyr pacakge.

table1 <- tidyr::table1
table2 <- tidyr::table2
table3 <- tidyr::table3

# The three tables  contain the exact same data, but in different formats.
# Let's have a look.
print(table1)
print(table2)
print(table3)

# table1 is the only table that is "tidy". Each variable has its own column,
# each observation its own row, and each cell represents 1 (and only 1) value.
# table2 suffers from being too long, and has individual cells that aren't
# values of an observation, but specify the variable that the value in the 
# adjacent cell should be applied to.

# Separate and Unite ----

# Let's work first on table 3 though, which has a more obvious (and more 
# easily fixed) problem than table2. The "rate" column actually contains two
# variables, the number of cases AND the population.
# This is breaking the 1st and 3rd principle of tidy data, as a column contains
# two variables, and each cell in that column has two values.

# We can fix this easily with the separate function, to split
# this variable into two or more different named variables.

tidy3 <- table3 %>% 
  tidyr::separate(rate, # the column we want to split
                  into = c("cases", "population"), # the new columns we want to split it into
                  sep = "/" # the character we want to split the variables on
)

print(tidy3)
# Note that separate will automatically find a character to
# separate on if you don't specify the sep argument (it will
# try to find a non-alpha-numeric character). However,
# it's better practice to be explicit about what to separate on.
# We can see that table3 now looks tidy, and satisfies the three
# principles of tidy data.

# If we look at our new dataset though, tidy3, we can see that 
# cases and population have been created as character variables.
# THis is signified by the "<chr>" below the variable name.
# This is not what we wanted as this will limit our ability
# to conduct analysis on what should be a numeric variable.
# There are two ways we could fix this: either we can change the 
# variable type after-the-fact, OR we can specify that the
# separate function should convert the data type for the new
# variables.

# Option 1, using "as" functions to change variable type
tidy3$cases <- as.integer(tidy3$cases)
tidy3$population <- as.integer(tidy3$population)

print(tidy3)

# we can see that this has worked by seeing the variable
# type under the variable name, or by doing some arithmatic 
# on the variables.
sum(tidy3$cases)
mean(tidy3$population)

# Option 2, allowing the separate function to assign new
# variable types.
tidy3 <- table3 %>% 
  tidyr::separate(rate,
                  into = c("cases", "population"), sep = "/",
                  convert = TRUE # allowing the function to guess new data types
)
print(tidy3)

# There is also the option of using the function type.convert() to allow R to
# guess the appropriate class for all variables in a data set. Note however,
# that this isn't always good practice, as R can make some decisions that we
# wouldn't - for example we might know that a variable should be able to take
# decimal points (so should be a detailed numeric class such as "float"), but 
# R might convert it to integer, which could cause headache trying to bind it
# with other data.

tidy3 <- type.convert(tidy3)

# We can also separate based on a set number of characters.
# to count from the left we use positive numbers, and from the 
# right we use negative numbers.

tidy3 <- tidy3 %>%
  tidyr::separate(year,
                  into = c("century", "year"),
                  sep = 2, # specifying to split after first 2 characters
                  convert = TRUE)
print(tidy3)

# Now obviously, this is a bad way to store a year variable 
# in MOST cicrumstances. We can easily pull these back together
# with the opposite function to the separate() function, unite().

test <- tidy3 %>% unite(year, # new column we want to create
                        century, year # names of columns we want to unite together
)
print(test)

# One thing to be careful of is that the unite function automatically
# puts in a separation character (an underscore). We can overwrite this
# by filling in the sep argument as blank ("")
test <- tidy3 %>% unite(year, century, year, sep = "")
print(test)


# Spread and Gather ----

# Separating and uniting columns is pretty easy to wrap your head around,
# as the number of rows stays the same. However, sometimes there will be 
# too many or too few rows in a data set. In other words, it wont be the 
# case that each row represents one, and ONLY one observation.
# A row might contain more than one observation, OR a single observation
# might be spread across multiple rows.
# With that in mind, let's look at table2.
print(table2)

# In this table, a single observation is split across two rows.
# "Afghanistan in 1999" should be one observation, with 4 variables:
# - country    = Afghanistan
# - year       = 1999
# - cases      = 745
# - population = 19987071

# Instead, we have a "type variable" that merely says what the "count" variable
# refers to. The classes in the type variable should instead by variables
# themselves, with the "count" being the value.

# We can fix this issue by using the pivot_wider() function from tidyr.
# This will essentially take a dataset and make it wider and (usually) shorter.

table2 <- table2 %>%
  tidyr::pivot_wider(names_from = type, # the column that will be become two (or more)
                            # columns based on the values of that column
                values_from = count # the column from which to take the values for the new columns
)
print(table2)

# Table2 is now tidy, with half as many rows, and each row representing
# a single, full, observation.
# Note that if we had had more than 2 values in "type" (for example, if gdp were
# included), then we would have more variables in our tidied table than in our 
# original table. Hence the name "pivot_wider" - in most cases it will take a  
# dataframe that is longer and narrower and produce one that is shorter and
# wider.

# Now let's have a look at the pivot_longer() function which we can think of as
# the opposite of pivot_wider

table4a <- tidyr::table4a
table4b <- tidyr::table4b

print(table4a)

# table4a contains the number of cases in a country in each year.
# It suffers from a very common problem for tables that have been
# designed to be "human readable" instead of "machine readable" - 
# that years are presented as multiple columns instead of making "year"
# a single variable.
# We can fix this with the gather function.
# Gather is the inverse of spread, as it takes a dataframe that is too wide
# and makes it longer. ie, it can take a dataset where a single row represents
# multiple observations, and turn it into one where a row representing a single 
# observation.

tidy4a <- pivot_longer(table4a, cols = 2:3, # which columns to manipulate
                       names_to = "year", # what to name our column with what 
                                          # were variable names
                       values_to = "cases"  # what to name our variable with the
                                         # values
                       ) 
print(tidy4a)

# Note that here we have made our dataset longer but not narrower.
# However, if the original untidy data had mutliple years (which
# is usually the case), we would have made the data much narrower.

# Exercise 1) ----
# Look at the gapminder_mess dataset and tidy it using the 
# functions introduced above.
# (remember that gapminder has data on life expectancy,
# population, and gdp).

gapminder_mess <- read_csv("data/gapminder_mess.csv")










# Missing Values ----

# Another common problem you will encounter with input data
# is the problem of missing data. Missing data is information that
# your dataset SHOULD have, but doesn't. However, it's
# important to be aware of the fact that there are two
# types of missing data:
# - Explicitly missing data
# - Implicitly missing data.
# Consider the below example

stocks <- read_csv("data/stocks.csv")
print(stocks)

# We have an EXPLICIT missing value for the return value of 
# quarter 4 of 2015. This is indicated by the "NA" value in the 
# data frame. We should know the return from Q4 of 2015, but we don't.
# However we also have an IMPLICIT missing value for the 
# return value of Quarter 1 of 2016. It's implicitly missing because
# there isn't any observation marked for it.

# Depending on what you're using your data for, it may be important to be 
# able to introduce implicitly missing values into your data.

# The pivot_wider() function can be useful for making implicit missing values
# explicit (even if it makes our tidy data untidy)

stocks_wide <- stocks %>% pivot_wider(names_from = qtr, values_from = return)
print(stocks_wide)

# We can see that we have now made our missing data for 
# qaurter 1 of 2016 explicit (ie - the missing value is represented
# in the data frame).
# We could pipe this pivot_wider() function back into a pivot_longer() function 
# to give us our original data frame, but with our explicit missing 
# data included.

stocks <- stocks %>%
  pivot_wider(names_from = qtr, values_from = return) %>%
  pivot_longer(names_to = "qtr",
               cols = c(`1`, `2`, `3`, `4`),
         values_to = "return")

print(stocks)

# We now have all our missing values explicitly listed.
# QUICK Q: how could you sort the dataframe so it ran in reverse
# chronology, with the most recent quarter first?
# (hint, think back to the first workshop)

# Replacing Specific Values ----

# Now let's say we investigated our source data, and we found out
# that an NA was inputted wherever the return was 0. This is a common
# error that can happen when taking data from one format to another, or from one
# software to another.
# We can replace NA values within a variable with the below bit of code
# using base R
stocks$return[is.na(stocks$return)] <- 0

# Here we're mixing up how we use our assignment operator. We're telling R to
# find all the places in stocks$returns where stocks$returns is NA, and then 
# over-writing this data with 0s.

print(stocks)

# Note that we could make this run across the whole data frame by 
# removing our variable specification
stocks[is.na(stocks)] <- 0

# Alternatively it might be the case that 0s (or some other value)
# have been loaded in where we actually don't know what the value
# should be. ie we should have NAs but don't. We can assign NAs
# in the same method as above

stocks[stocks == 0] <- NA

print(stocks)


# Fill blank space ----

# Another reason we could have NAs or blank entries in our data
# is if blank/NA values are supposed to be considered to be the 
# same as in the cell above them. This is another common problem
# in data that has been designed to be human readable instead of 
# machine readable. Consider the below example:

table6 <- read_csv("data/table6.csv")
print(table6)

# Table6 is almost identical to table1. The only difference
# is that there are NA values in the country column. However, it
# is obvious looking at it that these NA values should infact just
# carry the value of the nearest cell above that has a value inputted.

# We can fill these missing values with the fill function.

table6 <- table6 %>% fill(country)

print(table6)

# Manipulating Strings ----

# Character strings often require more tidying than numeric data, as
# there is more variability in how they could be inputted by
# manual data input.
# For manipualting character strings, we'll be using the stringr package,
# which is part of the tidyverse suite of packages.

# Consider the below sample data of kebab shops in London

kebabs <- read_csv("data/kebabs.csv")
print(kebabs)

# There is no consistency in how postcodes are formatted:
# some have lower case letters, some have spaces, some do not.
# If we want to be able to join these postcodes to other information
# (such as a postcode lookup tables to determine geolocations of
# each of these) then we will need to standardise how they appear.

# First we can remove the whitespace (spaces) from the values 
# with the str_replace_all function.

kebabs$POSTCODE <-str_replace_all(kebabs$POSTCODE,
                                  pattern = " ", replacement = "")
print(kebabs)

# Note that the str_replace function would behave similarly 
# BUT would only remove the first instance of a " " in a string
# so would not be useful if any of the strings have white space at the end
# of the variable, or had a double space.

# We can also fix all of the characters to be uppercase with the stringr 
# function toupper().
kebabs$POSTCODE <- str_to_upper(kebabs$POSTCODE)
print(kebabs)

# Our postcode variable is now nice and uniform.

# Another problem with this data frame is that the names of the 
# variables are inconsistent in how they use spacing and how they
# capitalise variables. It can also be quite annoying to work
# with variable names that have spaces in them, as this can often
# create errors in code if we try to call variable names directly
# in other functions. We could specify new names for our variables
# using the dplyr function rename(), OR we could use the janitor
# function clean_names() to change all of the names in a dataframe.
kebabs <- janitor::clean_names(kebabs, case = "snake")
print(kebabs)

# The "case" argument allows us to specify different standard 
# ways of capitalising letters and/or dealing with spaces.
# Some R users will tell you that variable names should ALWAYS be
# lower case, with no spaces (sometimes with underscores as spaces).
# I'm generally of the opinion that lower case variable names are preferable, 
# but there are some reasons you might capitalise variable names, such as to 
# distinguish acronyms, or you can capitalise the names of variables
# which have been "manipulated-in", while leaving the "original" variables
# lower-case.
# Different teams/users will have different rules and conventions for this, so 
# be sure you are aligned with your team.

# Dates and Times ----

# Another tricky variable type is data that represents
# dates or times. 
# In some circumstances, it will be appropriate to just keep 
# data that specifies times in a numeric format. For example,
# above we just kept the year variable as an integer.
# However, it's often the case that we have day-specific, and
# even time specific data, and keeping this data in an appropriate
# format is important for our analysis.
# R is able to hold information in the format <date>, <time>,
# or <dttm> (date-time), and these variables are often 
# manuipulated by leveraging the lubridate package.
library(lubridate)

# Let's explore this with a dataset representing 
# road accidents on scottish motorways in 2018.

accidents <- read_csv("data/accidents.csv")

print(accidents)

# We can see when we print the dataframe that the Date variable
# is actually being saved in R as a character variable. To turn
# it into a date format, we will need to specify it explicitly.

# There are a family of functions that can take character
# strings and turn them into date or time variables.
# Below we use the function (dmy) because our data is structured
# as "Day"/"Month"/"Year", but other functions in lubridate
# can accommodate a range of different structures for date and 
# time (eg mdy).

accidents$Date <- lubridate::dmy(accidents$Date)
print(accidents)

# You might get a warning message after running this 
# saying that R is unable to identify the current timezone.
# If this is the case, we can manually set the time zone
# with the following function. This is relatively unimportant
# unless we want to compare dates/times in our dataset with our
# current date/time.

Sys.setenv(TZ='UTC')

# The fact that R now recongises this as a date variable
# means that even though it appears as a recognisable
# character string for us, under the hood, R has saved it
# as a numeric variable.
# We can see this more clearly if we mutate the Date to the 
# integer which it is under the hood.
accidents %>% mutate(Date = as.numeric(Date)) %>% print()

# The "Date" variable is actually counted as the number of days
# since 1st January 1970. Time is similarly stored as the number
# of seconds since 1st January 1970.

# let's try make a "season" variable to see in what season accidents
# are most common

# we can set values for the start and end of each season
start_spring <- ymd("2018-02-15", tz = "UTC")
start_summer <- ymd("2018-05-15", tz = "UTC")
start_autumn <- ymd("2018-08-15", tz = "UTC")
start_winter <- ymd("2018-11-15", tz = "UTC")

# Then we can use these values for a mutate and case_when function
accidents <- accidents %>%
  mutate(season = dplyr::case_when(
    Date >= start_spring & Date < start_summer ~ "Spring",
    Date >= start_summer & Date < start_autumn ~ "Summer",
    Date >= start_autumn & Date < start_winter ~ "Autumn",
     TRUE ~ "Winter"))

# Finally we can count the number of accidents in each season
accidents %>% group_by(season) %>% tally()


# Exercise 2) Use the above method to determine the times of
# day that accidents happen.
# Note: you can do this in multiple ways. Think about how you might
# group by hour, or group by daytime/night-time etc.
# Have a google to see if there are any useful functions to group by hour.









# Working With Excel Data ----

# A common problem for analysts looking to implement R in their day-to-day
# analysis is that a lot of their work is wrapped up in another software.
# Generally speaking, softwares like Stata, SPSS, and SAS typically work
# with tidy data frames very similar to what you want for working in R, making
# it easier to move work from one of these to R.
# However, Excel can be quite different. Workbooks can have multiple sheets,
# a single sheet can have multiple tables, and tables can have lots of 
# "design" features that might make them easier for humans to read, but make
# them much harder for machines to read (blank rows and columns, merged cells,
# variable names identified across multiple cells etc.)

# It can be tempting to try to tidy up a workbook in excel before bringing into
# R, but this introduces room for human error, makes QA of work more time
# consuming and difficult, and means we can't repeat the process quickly when
# we want to do it again - all things we are trying to avoid by using R!

# It's also useful to do this as you may find yourself in a situation where you
# have to change a large analytical product segment by segment, and you will 
# need a way to get R to interact with Excel while your moving the work
# across.

# Let's imagine we have a model output from an Excel model where we want to 
# start analysing the results in R instead of Excel (ahead of moving the whole
# model into R of course).

# We can read in Excel data with the readxl() function the read_excel package.

df_untidy <- read_excel("data/Model_Output.xlsx",col_names = FALSE)

# Let's have a look at our untidy Excel input
View(df_untidy)

# We can see a few things:
#     - cell [1.1] tells us the model run name
#     - the data frame only begins in cell [3,7]
#     - the variable names are in row 5
#     - there are a number of blank rows at the bottom of the table


# Let's start by saving the model run name in cell [1,1] as we might want
# to use this later.
df_name <- pull(df_untidy[1,1])

# Next, let's remove unneeded rows and columns
# We can do this by specifying a number of rows and columns to "subtract" from 
# our dataframe. We point to specific cells in R by using the syntax
# [row, column], but we can point to a whole row or rows at a time by leaving 
# the column blank. eg [2 , ] would be row 2. 

# Remove first six rows
df_untidy <- df_untidy[-c(1, 2, 3, 4, 6) , ]

# Remove first two columns
df_untidy <- df_untidy[ , -c(1, 2)]

# We now have an NA in the first row (which seems to be a unique id for each
# entry). Let's assign a value for it
df_untidy[1,1] <- "id"

# Next, let's take the first row and turn those into variable names
df_untidy <- df_untidy %>% janitor::row_to_names(row_number = 1)

# Some of the variable names have spaces in them which will be annoying for 
# working with in R, so we can use a janitor package function to fix the names

df_untidy <- df_untidy %>% janitor::clean_names(case = "snake")

# Now we can easily remove the rows which have blank entries

df_untidy <- df_untidy %>% filter(!is.na(region) & !is.na(state_spend))

# One remaining problem is that the date variable is a number (representing how
# Excel counts dates under the hood). We should be careful not to use a 
# lubridate function right away as R and Excel count dates differently (day 1 in
# Excel is 1/1/1900 while day 1 in R is 1/1/1970).
# We can change the date by first making sure the entries are integer (instead
# of character), before using the janitor function excel_numeric_to_date()

# First, make int
df_untidy$date <- as.numeric(df_untidy$date)

# Then fix excel dates
df_untidy$date <-janitor::excel_numeric_to_date(df_untidy$date,
                                                date_system = "modern",
                                                include_time = "FALSE")

# Finally, we  can rename our dataframe based on the model run number we pulled
# out from our untidy dataframe we began with

# We do this by using the assign function, assigning the value of df_name
# to our dataframe
assign(df_name, df_untidy)

# We now have a tidy dataframe named after the model run.

# Assuming that our output was the same each time, we could always use this
# same chunk of code.



# Some other useful stuff ----

# As you develop your R skills, it's worth trying to build up a suite of 
# functions in your head that you can always rely on to work out how your data
# looks, so you can understand it and work out what needs to be done to it.
# Functions like summary() - for summaries of data frames or specific variables,
# class() - which gives the type of an object, and head() - which gives the top
# n rows of a data frame, are good places to start. But, you might find your own
# or develop ones specific to work you do.


# FINAL EXERCISE

# Import some messy data of your own. Work out how to get it working in R as a 
# tidy data frame (if you need to do one or two bits in Excel first, that's
# okay)

# Further resources ----

# 1) We talked here about how and why you should make your
# data tidy. However, there are legitimate reasons why you
# might keep your data in a "non-tidy" way. This could be 
# for performing specific tasks, for exporting human-readable
# spreadsheets, for data visualisations or for more space-efficient storage.
# To learn more about this, check this blog post on the uses
# of non-tidy data: https://simplystatistics.org/2016/02/17/non-tidy-data/


# 2) For a number of useful functions for working with excel data, see the
# following Github with some function for working with excel
# https://github.com/yusuzech/tidyverse_notes/blob/master/utility/read_excel_tables.md

# 3) Martine Wauben in gave a presentation on importing data for our in-house
# Coffee and Coding group. It went into more depth than we did here and has
# more tips and tricks than what's here.
# You can find the code for the presentation here:
# https://github.com/DataS-DHSC/coffee-and-coding/tree/master/2020-09-10 Importing data



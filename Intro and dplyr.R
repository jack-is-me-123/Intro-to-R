###  Workshop 1 - Intro and dplyr

# Author: Jack Marks
# Title: R for Beginners: Workshop 1
# Purpose of the workshop: Setting up a project, understanding
# different data types, 
# loading in data, dplyr manipulation
# Date: July 2020


# It's good practice to write your name and the date at the beginning of your
# script, as well as a title and short description of what the script will set
# out to do. You can "comment out" anything that you want to write in the script
# that you don't want to be part of your code by using '#'. This basically tells
# R to ignore everything that come after a "#".

# You can also make your code easier to navigate by putting four dashes '-' 
# to put bookmarks in R scripts. These can be navigated to at the bottom of the 
# script panel of the R studio console.

# Running Code ----
# You can run a piece of R code in a number of ways. You can highlight a whole
# chunk of your R script and press "Ctrl + Enter" or use the "Run" button at the
# top right of the script panel. You can also run individual lines at a time by
# clicking the end of a line of R code and pressing "Ctrl + Enter".

3 + 4

# Packages ----

# One of the things that makes R so versatile is its active developer community.
# Any R user is able to create new R functions and share them with other R users
# to extend R's capability. These user generated functions are shared in sets
# known as "packages" which have to be installed on your R system to use.

# If you're using R for the first time, you will have to install packages with 
# the following code. We'll be installing the tidyverse, which is a large suite
# of packages that contain lots of useful stuff for data manipulation, data
# visualisation, reading in data etc.

# NB You don't have to run this every time: installation can take a couple of 
# minutes depeneding on the size of the package.

install.packages("tidyverse")
install.packages("RODBC")

# Once the packages are installed on your machine you need to load them in. You
# can do this using the 'library' function .Try running the below bit of code.
# Have a look at the console (below) to see what happens.

library(dplyr) #manipulating data
library(readr) #reading in data
library(ggplot2) #creating plots


# You may get a warning message saying what version of packages you're getting.
# You can generally ignore these for now (a big part of learning to use R is 
# getting a sense of which warning messages  are bad and which ones can be 
# ignored).

# R is basically a calculator ----

# In essence, most of what you do in R will be some form of arithmetic. As such,
# we can use a suit of regular arithmetic expressions  and use R as a basic 
# calculator.

# Addition: +
# Subtraction: -
# Multiplication: *
# Division: /
# Raising to the power of: ^ 

# Try  running the following lines of script:
3+8 
54/2 
98-3
1 / 200 * 30
3^3

# Try out some of the other basic arithmetic operators. It's good to know that R
# can do this, as at some point you might want to create a new column in your
# data set using your own calculation, for example, to make a proportion
# variable.

# Objects in R ----

# Another basic concept in R is the creation of an object. Objects allow you to
# store information, so that they can be used later. An object can be a single
# value, a data frame, a vector, a value etc. (we'll touch on these different
# data types throughout the course).

# When creating an object in R, we use the assignment operator '<-' 
# e.g. object_name <- value

an_object <- 123

# R has five basic or atomic classes of objects:
#  character
#  numeric (real numbers)
#  integer (whole numbers)
#  complex
#  logical (True/False)

# run the object below. Once run the object should appear in the 'values'
# area in your global environment to the right. 

my_value <- 17

# now type 'my_value'into the console below and hit return.
# The console should print the value
# of the object


# Now create a new object

my_new_value <- 36-9

# You can now use the values together. 
# If you run this line, R should print the value of my_new_value minus my_value 

my_new_value - my_value

# Vectors ----

# A basic type of R object is a vector.
# A vector is a one dimensional array of data. It can hold numeric data,
# character data, or logical data, but all the data in the vector must be of the 
# same type.
# Empty vectors can be created with the vector() function.

x <-  vector() 

numeric_vector <- c(5, 12, 98)
x <- numeric_vector
x

character_vector <- c("hello", "my", "name", "is", "Jack")

# Similar to vectors, lists are also a one dimensional array of data, however
# unlike a vector, a list can hold data of different classes. In fact, this is 
# usually why we would use a list. We can also add categories to our lis

# You can also add names or column headers to a vector with the names() function

#Have a look at this example:

a_vector <- c("Cat", "Analyst", 7)
names(a_vector) <- c("Animal Type", "Profession", "Grade")

#This means that the name 'cat' is assigned to the animal type,
# and 'maritime mascot' is assigned to the profession 
print(a_vector)

# LOADING IN DATA ----

# Let's load in some data that we can play with.
# It's a csv file, so we can use 'read_csv' from the readr package. We first set
# our working directory. This is a way of telling R where in our file structure
# we're operating. When we do this, it means that we wont have to give full file
# paths when loading in data, but instead can just direct from the location of
# the working directory.

# To do this, we're going to go to the 'Session' button in the ribbon menu and
# select 'Set Working Directory > To Source File Location'. This will set our
# working directory to the location of this script. This means that when we want
# to load in our dataset, we just need to navigate from here, rather than from
# the beginning of the file structure.

# Let's load in some data.

gapminder <- readr::read_csv(file = "data/gapminder.csv")

# This dataframe object is now loaded in our global environment (top right).
# read_csv() uses the first line of the data for the column names. 
# If we were working with a csv that didn't have named columns, we could choose
# to make the first row of the csv a row of data by setting the argument
# col_names = FALSE - so the whole function would read:
# gapminder <- readr::read_csv(file = "X.csv", col_names = FALSE)

# Let's have a look at our data (using a couple of different functions)
dplyr::glimpse(gapminder)

# look at the top 6 rows of data
head(gapminder)

# look at bottom 6 rows of data
tail(gapminder)

# print the data to console (limit of 10 rows)
print(gapminder)

# print ALL of the data to the console by setting an infinite limit to print()
# (This is not recommended in most circumstances unless you want to fill up your
# console very quickly)
print(gapminder, n = Inf)

# view data
View(gapminder) # note the capital 'V'

# summary {base} stats
base::summary(gapminder)

# You can also inspect a single variable by using the $ sign after the dataframe
# to specify a single variable
base::summary(gapminder$lifeExp)

# We can use the function unique() to get unique values of a certain variable
base::unique(gapminder$country)

# we can also use square brackets to specify specific rows, columns, or cells. 
# This is done in the format: dataframe[row,column]
# if we leave either the row or column blank, we get the whole column or row.

print(gapminder[,4])
print(gapminder [2,3])

# Manipulating data ----

# Pretty much any project you do in R is going to involve manipulating data in 
# some way. Whether it's creating new variables (based on existing variables), 
# filtering out data or joining in new data.

# The main "verbs" we get with the dplyr package are:
# Pick variables by their names - select()
# Pick observations by their values - filter()
# Create new variables based on existing variables - mutate()
# Reorder the rows - arrange()
# Join dataframes together - _join() and _bind() functions
# Collapse many values down to a single summary - summarise()


# All verbs (or "functions" as they're known in R) in the dplyr
# family (and most functions in R from any package) work similarly:

# 1 The first argument is an object you want to manipulate in some way, such as
# a data frame.

# 2 The subsequent arguments describe what to do with the object,
# using the variable names (without quotes). This can include dropping
# variables, adding a new variable (based on other variables), filtering out
# observations (rows) based on some criteria, or rearranging your data (for
# example from highest to lowest of a variable).

# 3 The result is a new object. This object is either printed directly
# into the console (the panel below this script), OR, if it is saved as a new
# object (using the "<-" assignment operator), it is saved as a new item in the
# global environment  (to the right of this script) and can be used later.

# select() ----

# The select() function lets us select variables to keep and variables to get 
# rid of. In other words, we use select to remove columns from a data frame.

countries_pop <- dplyr::select( 
  gapminder,  # the first argument is always the data
  country, # the other arguments are column names you want to keep
  pop)  

View(countries_pop)

# We can also put a "-" sign in front of variables we don't want to keep in our
# dataframe. doing this tells R that we want to keep all the variables except
# for the variable(s) we select out.
select(
  gapminder,
  -gdpPercap  # columns to drop
)


# we can also pick columns based on some criteria. Here, we only keep columns
# that contain the letter "y"
select(gapminder, contains("y"))



# CHALLENGE 1!
# Create an object called my_selection that uses the select() function 
# to store from gapminder the country column and any columns that end with 
# with "p"





# filter() ----

# The filter function lets you filter out rows of a data frame based on some
# criteria. This criteria is based on some rule defined by a logical operator.
# For example, whether a certain variable is above or equal to a certain value.
# We can use a range of logical operators to filter:

# ==      equals
# !=      not equals
# %in%    match to several things listed within a vector c()
# >       greater than
# >=      greater than or equal to
# <       less than
# <=      less than or equal to
# &       and (for combining multiple logical operators where ALL conditions 
#         have to be met)
# |       or (for combining multiple logical operators where at least one 
#         condition has to be met)  

# NB the double equal sign (==) for the "equals" logical operator. A single
# = sign is used a lot elsewhere in R.

# filter on a single variable
dplyr::filter(gapminder, year == 2007) # numeric variables don't need quotations 

# fitler on more than one variables
dplyr::filter(gapminder,
              year == 2007 & continent == "Asia")

# A reminder, if we wanted to save this output to our global environment, we 
# would use the assingment operator (<-)
Asia_2007 <- filter(gapminder, year == 2007 & continent == "Asia") 

# now everything except for one continent
filter(gapminder, continent != "Asia")

# filter on three countries and one year
# %in% lets us match on multiple value
filter(
  gapminder,
  country %in% c("Albania", "France", "Norway") & year == 2002 
)

# or a numeric example with lots of conditions (try to figure out exactly
# what this function is doing without looking at the output )
filter(gapminder,
       year == 2002 & pop >= 100000000 | year == 2002 & pop <= 1000000)

# These may seem like arbitrary tasks here, but you can imagine
# how the filter and select functions could be used together to quickly
# strip down your data to only to what's needed for a certain task, or to make
# a certain table output for your work (similar to using pivot tables in Excel).


# CHALLENGE 2----

# Filter the gapminder dataframe to only
# those countries that are either in Europe or Africa
# and had a population between 5 and 10 million
# in 2002


# MUTATE----

# The mutate function creates a new column in your data
# You can either use your current variables to assign values of a new variable
# or assign your whole data frame with the same value of a new variable

# Let's add a column for gdp , where we multiply gdpPercap by population
gapminder<- dplyr::mutate(
  gapminder,  # data we are mutating
  gdp = gdpPercap * pop  # new column from old ones
)

# let's see the gdp variable
base::summary(gapminder$gdp)


# You can also mutate with an if_else() function nested inside your mutate
# function:
mutate(
  Asia_2007,
  healthy = if_else(
    condition = lifeExp >= 50,
    true = "yes",  # ...fill column with this string if true
    false = "no"  # ...otherwise fill it with this string
  )
)

# And we can get more nuanced by using a case_when() function
# This prevents us writing nested if_else() 
# statements to specify multiple conditions (a relief to anyone who's ever had 
# multiple IF statements within eachother in Excel).

# How the cae when function works is it will go though each logical argument for
# each piece of input data (here, a row of data), and assign a value the first 
# time a logical statement passes.

mutate(
  Asia_2007,  # data
  health = dplyr::case_when(
    lifeExp <= 45 ~ "very_bad",
    lifeExp <= 55 ~ "bad",
    lifeExp <= 60 ~ "okay",
    lifeExp <= 70 ~ "good",
    TRUE ~ "very_good" # This last category is equivalent to saying "else"
  )
)

# Challenge ----

# Write a mutate function that takes the Asia_2007 data frame and classifies 
# countries by whether they have bad, okay, good, or great living conditions.
# What variables would you use for this?  (Remember the case_when function, and
# play around with the and/or operators to add logical conditions to a test "&"
# "|")

#ARRANGE----

# This alters the order of the rows in your table according to some criteria

dplyr::arrange(
  Asia_2007,  # again, data first
  pop  # column to order by
)

#And in reverse order (largest pop first):
arrange(Asia_2007, desc(pop))  # descending

# CHALLENGE 3!
# What happens if you arrange by a column containing characters rather 
# than numbers? For example, the country column.


# JOIN----

# This is another verb that mirrors what you can find in SQL. There are 
# several types of join, but we're going to look at the most common one:
# the left_join(). This joins information from one table x to 
# another table y by some key matching variable of our choice.
# 
# Lets start by reading in some more data to use
# we're going to use Freedom House scores.
# Freedom House rates countries on Civil Liberties and Political Rights,
# and uses these scores to categorise countries as "Free", "Partly Free" or
# "Not Free"

freedom_house <- read_csv("data/freedom_house.csv")

# let's inspect its contents
glimpse(freedom_house)

head(freedom_house)

# We can see that two of the variable names have spaces in them

# Now we're going to join this new data to our gapminder data. The key 
# to matching these is in the country column, which exists in both datasets.

gapminder_join <- dplyr::left_join(
  x = gapminder,  # take this table...
  y = freedom_house,   # ...and join this table
  by = "country"  # on this key/variable
)

glimpse(gapminder_join)

# We can use multiple variables in the "by". Alternatively, we can
# leave it blank and R will guess what variable(s) to join on if there are
# common variable names.

# It's always good practice to make sure before joining two dataframes that the 
# only variable names they share are for variables that express the same value.
# You wouldn't want to join two dataframes that both had a variable called
# "score" if those scores where for different things. If you find yourself in 
# this situation, you should rename some variables with the rename() function.
# This takes the structure of rename(data, new_name = old_name). eg
rename(gapminder, GDP = gdp)

# BIND----

# Sometimes instead of joing two data frames horizontally (joining)
# you might want to join them vertically (binding)
# In other words, instead of adding variables/columns to your data frame,
# you want to add more observations/rows to your data frame from a data frame 
# with (some or all) of the same variables

# To illustrate this, we're going to add a data frame of fictional countries
# to our gapminder data

# first, we'll read in the data
fictional <- read_csv("Data/fictional_countries.csv")

# Inspect the data
View(fictional)

# Now we can bind the fictional countries to our gapminder dataset
# First though, we'll make a gapminder data set with just 2007 entries
gapminder_2007 <- filter(gapminder, year == 2007)

# Now we use the rbind function to bind the tables
gapminder_2007 <- rbind(gapminder_2007, fictional)

View(gapminder_2007)

# we can find our fictional countries down at the bottom of the dataframe


# This workshop does not contain an exhaustive list of other functions 
# within the same family as select(), filter(), mutate(), arrange() and
# *_join(). There are other functions that will be useful for your work 
# and other ways of manipulating your data. We will look at some of these 
# in the next workshop.


# PIPES ----
# Alright great, we've seen how to manipulate our data a bit. But 
# we've been doing it one discrete step at a time.
# If you wanted to do some more complex data manipulations (using more than one
# function on some data), your script might end up looking something like this:

gapminder_filter <- filter(gapminder_join, year == 2007)

gapminder_select <- select(gapminder_filter, -status, -year)

gapminder_mutate <-
  mutate(gapminder_select, happiness = gdpPercap * civ_rights / 10000)

# In other words, you might end up creating lots of intermediate 
# objects - cluttering up your workspace and scripts, and filling up memory.
# 
# You could do all this in one step by nesting each function inside 
# the others, but that would be quite messy and hard to read.
# This is because R essentially reads from the inner-most function out (like
# functions in Excel or in another coding language).

gapminder_mutate <-
  mutate(select(filter(gapminder_join, year == 2007), -status, -year),
         happiness = gdpPercap * civ_rights / 10000)

# The dplyr package has a very useful feature called "pipes" (written as %>%).
# With pipes, you can take the output of one function
# and use it as the first argument of the next function.
# In other words, the pipe (%>%) lets us read our code from left-to-right 
# (or from top-to-bottom) instead of reading from the inside out
# without having to create lots of intermediate items.
# 
# We can use the pipe to do the same thing we did above

gapminder_piped <- gapminder_join %>%
  filter(year == 2007) %>% # we don't need to put the data argument in this function as it's been piped in
  select(-status, -year) %>%
  mutate(happiness = gdpPercap * civ_rights / 10000)


head(gapminder_piped)

# This reads as:
# make an object named gapminder_piped by: take the gapminder_join dataframe %>%
# filter for observations where year is 2007 %>%
# then remove (select out) certain columns %>%
# then make a new column called "happiness" based on gdp per capita and civil 
# liberties
#   
# We didn't have to keep calling the dataframe object 
# in each function call. For example, we used filter(year == 2007) rather than  
# filter(gapminder_join , year == 2007) because the data argument was piped in. 
# The functions mentioned above all accept the data being passed into them 
# because they're part of the tidyverse (a large suite of packages designed to 
# be compatible with dplyr).
# SOME functions and data types wont take piped arguments, but as commonly used 
# packages are updated this is vanishingly rare in R.

# CHALLENGE 5 ----
# Write a piped operation that creates a new dataframe called small_and_free
# that takes the gapminder dataframe and:
#   - left_join()s the freedom_house dataframe by country
#   - selects only the variables "country", "population" and "Status"
#   - filter()s by those with a population less than 5 million AND a Status of 
#     "F"in 2007
#   - arrange()s the dataset by population (in ascending order)


# SUMMARIES ----
# Assuming we've now wrangled out data using the dplyr functions, we can do some 
# quick, readable summarisation that's way better than the summary() function.
# So let's use our knowledge and some new functions to get the count
# of countries per continent.



gapminder %>%  # take the dataframe
  dplyr::filter(year == 2007) %>% # filter only for 2007 data
  dplyr::group_by(continent) %>%   # group it by continent
  dplyr::tally() %>%   # tally up (count) the number of instances
  dplyr::arrange(desc(n)) # arrange in descending order

# The order of your functions is important - remember it's like a recipe. Don't
# crack  the eggs on your cake just before serving. Do it near the beginning
# somewhere, I guess (I'm not much of a baker - though I love Bakeoff).
# 
# There's also a specific summarise() function that allows you to, well,
# summarise. It basically lets us do everything which we could do with Excel 
# Pivot tables.

# Say we want to count the number of countries in each continent
# and their average population:

gapminder_join %>%  # take the dataframe
  dplyr::filter(year == 2007) %>% # filter by year
  dplyr::group_by(continent) %>%   # group by variable
  dplyr::summarise( # summarise it by...
    count = n(),  # counting the number
    mean_pop = mean(pop)  # and taking a mean to nearest whole number
  ) %>% 
  dplyr::arrange(desc(mean_pop))  # then organise in descending order of this column

# We can have any number of bits of information in the summarise() function,
# basically listing the variables of the small table we want to produce and how
# R should calculate those variables.

# We can also add more groupings, grouping by both continent AND freedom status.

gapminder_join %>%
  filter(year == 2007) %>%
  group_by(status, continent) %>% 
  summarise(
    mean_pop = mean(pop),
    count = n()
  ) %>%
  arrange(desc(count))

# It's worth noting that how R handles groups under the hood is that when you
# use a group_by() function, R makes a "grouped tibble". Without going into too
# detail on this, it means it will behave slightly differently to a regular 
# dataframe. If we run a function on a grouped tibble, it will run that function
# on each group. This lets us do things like filter for the maximum value of
# a variable in each group. For example, we can extract the country that has the
# highest population in each group as follows
gapminder_join %>% 
  filter(year == 2007) %>%
  group_by(continent) %>%
  filter(pop == max(pop))


# Challenge ----
# Play around with group_b and summarise functions.
# Find out how you would use group_by to get the country in each
# continent with the lowest gdpPercap in 2007



#Some Other Useful Stuff----

# That's it for the first workshop. You should now feel comfortable with some
# of the basics of R. The best way to learn it is to do it, so have a play
# around with the gapminder dataset (or maybe try copy in some of your own 
# data) and think about other ways you might try edit it using the functions
# introduced above.

# Throughout this course, we will point you to extra resources which you might 
# find useful. These resources are not necessary to following the course, but
# will contain useful reosurces for those who want to develop more R skills more
# quickly.


### 1. DELETING OBJECTS ----

# you can remove data frames, vectors or other objects from your global 
# environment either by using the "grid" selection options in the Environment
# (in the top right of the R console there's a drop-down item for "list". Change
# this to "grid" and selection which objects you want to delete)

# OR you can do it in R code by using the function remove()
# for example
remove(gapminder_mutate)

# Notice that gapminder_mutate just popped out of existence
# you can use this on multiple objects at once
remove(gapminder_filter, gapminder_select)

# This can be useful in long automated processes, so that you can keep your R
# global environment clean.


### 2. EXPORTING CSVs ----

# You can export your tidied R dataframes as csvs using the function write_csv()
# The first argument is your data frame. the second argument is the file path 
# (directory) you want to save it in

readr::write_csv(gapminder_join, "data/gapminder2.csv")

# note that write_csv() comes from the readr package. There is also a base r 
# function write.csv(), which behaves similarly, but the readr version is 
# generally a touch more clever and will give slightly more informative error 
# messages if anything goes wrong.

### 3. CALLING PACKAGES ----

# You may recall that at the start of the workshop, we used the function 
# library() to load in the packages we would be using for this workshop. This is
# because we needed to have these packages loaded in for functions in those 
# packages to work.
# However, if you work on projects where you use lots of different packages, it
# can be hard to keep track of what packages each function comes from. This can
# become an issue if two packages have the same function name for two functions
# that do different things. For example, there are a number of packages that
# have a function called "plot()" that will behave differently.
# To avoid this, it's best practice to specify what package you want
# R to use before calling the function. This means R knows exactly what to 
# do AND it makes it easier for other people to QA your code.
# For example, rather than just calling the function "filter()", we 
# would type "dplyr::filter()

dplyr::filter(gapminder, year== 2002)

# Another benefit of calling these packages directly is that you don't need
# to have loaded a package if you use this method, they just need to be 
# installed. This can be very useful when 
# you only need one function from a package as it reduces the risk of calling
# the wrong function elsewhere in your script.


### 4. USING AND CREATING R PROJECTS ----

# A project is simply a way of telling the R console where you are working, and
# lets you save objects to your global environment for your next session.

# We can also set a working directory for our work by going to the Session
# tab at the top of the page and specifying our working directory, 
# or by using the function setwd().

# Similarly, when we export a csv, the R project will automatically export the 
# csv into the current working directory.
# By default, this will be the folder the project is linked to (unless we 
# specify otherwise).

# To create an R project, just use the drop down in the top right of R studio
# and select the "New Project" option. Then navigate to the folder you want 
# your R project to sit in.

# Some people love working with R projects, others don't.

# 5. Styling your R studio ----
# R studio provides a lot of options for changing the look and feel of R studio.
# Go to the Ribbon menu at the top and click 'Tools' before going to 'Global
# Options'. Here you can change the Appearance letting you change the font size
# and theme of R studio. Personally, I'm a fan of the Cobalt theme, but you 
# should play around with different themes and see which you find it easiest to
# work in.
# You can also apply a margin to your r scripts. It can be useful to  have a
# line that shows when you've hit 80 characters in a line. In code writing, it's
# often considered best practice to limit a line of code to 80 characters. This
# has its roots in very early coding languages which had 80 characters per line
# as a hard limit, but has survived as a coding convention. It's okay to break 
# this limit occasionally (for eg when calling a long file path), but generally
# your code will be easier to read if you stick to the 80 character limit.
# You can edit this in Global Options > > Code > Display > Show Margin, and 
# setting "margin column" to 80.

# EXTRA MATERIAL----

# Below are some extra sources you may want to look at if you want to explore
# any of these concepts further. None of them are necessary if you
# want to follow this intro to R course, but they may prove helpful as you
# develop your R skills.

# 1. https://www.datacamp.com/community/tutorials/functions-in-r-a-tutorial#what_are
# This blog post goes into some more detail on the basics of R functions without
# getting two technical. It also teaches you how to write your own
# R functions and use them in your data analysis! (We will be looking at writing
# functions in the third workshop)


# 2. 'Beginner R and RStudio training' by Matt Dray (DfE)
# at the following web address
# https://matt-dray.github.io/beginner-r-feat-pkmn/#5_get_data_in_and_look_at_it
# This course borrows heavily 

# 3. A workhshop on the dplyr 5 verbs at the following web address:
# https://teachingr.com/content/the-5-verbs-of-dplyr/the-5-verbs-of-dplyr-article.html


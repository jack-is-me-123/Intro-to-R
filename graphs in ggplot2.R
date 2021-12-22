###  Workshop 4 - graphs and ggplot2

# Author: Jack Marks
# Date: September 2020

# This workshop will look at how we can use R to visualise data, drawing
# graphs and plots to visualise data distributions. 
# These can be used to reliably and quickly produce sets of graphs from data
# that comes in in a regular format. 
# Graphs can also be useful for 
# We will be using the package ggplot2 for this, but will touch on some other 
# resources that exist for plotting data in R, including base R and the plotly
# package.

#The ggplot2 cheat sheet can be useful in understanding how ggplot works. 
#Cheat sheets are available for most packages and a really good 'desk aids'. 

# Load packages

library(tidyverse) #manipulating data
library(ggplot2) #creating plots
library(readr) #reading in data
library(ggthemes) # includes a range of extra themes for plots
library(RColorBrewer) # includes a range of additional colour palettes
library(plotly) # package for producing interactive plots


# We're going to work with some data we've used in previous workshops

gapminder <- readr::read_csv(file = "data/gapminder.csv")
exams <- read_csv("data/exams.csv")

# What is ggplot2 ----

# ggplot2 is a package that has a whole range of data visualisation tools. The 
# "gg" stands for "grammar of graphics". This is a design philosphy to making
# plots where you build graphs by layering graphical elements on top of
# eachother. To do this, we use a '+' rather than a pipe. Think about it as 
# placing down a canvas and then adding layers on top.

# Let's begin with a simple but useful function, qplot(). 
# qplot() let's us make plots very quickly with very few arguments. It can be a
# useful tool when playing around with data to make sure that data makes sense.
# It can sometimes be the quickest way to get to know your data or identify 
# if errors have occured, or if you have any outliers.

# Let's use qplot() to look at some exam scores
qplot(data = exams, x = math)

# without us defining a type of plot, qplot guesses what sort of graph to create
# based on the inputs. While the graph isn't particularly pretty, it can help us
# get our heads around our data quickly.

# Let's see what happens when we run qplot on two variables
qplot(data = exams, x = english, y = math)

# It's important to note that as we create plots, we can toggle between them
# with the arrows in the plot viewer. Having too many plots or elements in the 
# plots or viewer window can slow down R studio a bit so it can be useful to 
# clear objects from time to time. You can delete a single object from the 
# viewer pane by clicking the "X" next to "Export". OR you can clear all objects
# in the viewer pane by selecting the brush.

# R assumes we want a scatter plot, which is a useful way for us to see how 
# two variables interact.
# We can even add a colour as a third parameter
qplot(data = exams, x = english, y = math, colour = history)

# Quick challenge:
# Use the qplot() function to explore the gapminder dataset

# ggplot2: single variable graphics ----

# When building up a ggplot2() graphic, we begin with the function ggplot().
# Inside this function, we specify what data we are using, as well as define
# what variables of that data will be matched to what aesthetics (inside the 
# aes() bracket).
# we then add a plus '+' sign to tell ggplot we want to add a layer. We can then
# define a graphic to apply. Here, we'll use the function geom_histogram() to 
# make a histogram.

ggplot(data = exams, aes(x=math)) + 
  geom_histogram()

# We can keep our first line the same but change the function in the second line
# to produce a different type of chart.

ggplot(data = exams, aes(x=math)) + 
  geom_density()

# building up aesthetics ----

# Let's have a go at making our graph look a bit better.
# We'll start with the histogram

ggplot(data = exams, aes(x=math)) + 
  geom_histogram()

# First thing we might want to do is change the number of "bins" (columns) in 
# the histogram. We can do this by defining the number of bins in our 
# geom_histogram() function.

ggplot(data = exams, aes(x=math)) + 
  geom_histogram(bins = 25)

# Next, let's add some colour. We can define colours in many ways in R. One
# option is to put a colour in quotation marks (eg "black", "blue" etc). Another
# option we will explore later involves using loaded in colour palettes such as
# those in rcolorbrewer. Here, we'll be defining our colours with hex codes.
# There are some good online tools for exploring hex colour codes. 
# eg: https://www.color-hex.com/


ggplot(data = exams, aes(x=math)) + 
  geom_histogram(fill = "#76a8d5", bins = 25)

# Note that we use the fill argument to fill something with a colour. Calling 
# 'colour' would actually change the outline colour.

ggplot(data = exams, aes(x=math)) + 
  geom_histogram(colour = "#76a8d5", bins = 25)

# Many of the aesthetic arguments in ggplot2 functions are the same across
# different types of graphs.

ggplot(data = exams, aes(x=math)) + 
  geom_density(fill = "#76a8d5", colour = "#76a8d5")

# Next, let's get rid of that awful grey background. We can do this in two ways.
# The easiest way to do that is to add another layer to our ggplot defining a 
# "theme". There are loads of themes that come included in ggplot2 and other
# supported packages.

ggplot(data = exams, aes(x=math)) + 
  geom_density(fill = "#76a8d5", colour = "#76a8d5") +
  theme_classic()

# Here we'll just use the "classic" theme for simplicity, but there are loads of 
# fun themes out there. Some newspapers, businesses and government departments 
# have even created themes that let them quickly produce graphics in their own
# house style which you can borrow.

ggplot(data = exams, aes(x=math)) + 
  geom_histogram(fill = "#f08080", colour = "#f08080") +
  theme_economist()

# We can add transparency to our graphic using the "alpha" argument. alpha takes
# a value between 0 and 1 - 1 being totally opaque, and 0 being totally
# transparent. This can be especially useful when points in a scatterplot
# overlap, or for displaying multiple variables at once in a histogram/density
# plot.

ggplot(data = exams, aes(x=math)) + 
  geom_density(fill = "#76a8d5", colour = "#76a8d5", alpha = 0.4) +
  theme_classic()

# The geom_histogram and geom_density functions are quite flexible, allowing us
# to plot one variable but for multiple groupings of our data. Let's see this in
# action by plotting exam results for students based on whether they did a test
# prep course. We can do this by adding a "group" argument to our aes().
# What this does, it it will first group our data by whether a student had a
# prep course, and then it will plot a density plot for every group (ie students
# who have and students who have not completed a test prep course).

ggplot(data = exams,
       aes(x=math, group = test_prep_course, fill = test_prep_course)) + 
  geom_density(alpha = 0.4) +
  theme_classic()

# Challenge:
# Use ggplot2 histograms and density charts to explore the gapminder dataset.
#   - Try using some groupings to better understand how different variables
#     interact.

# Pipes, data manipulations, and ggplot2 ----

# It would be useful to produce this kind of graph for the results in different
# subjects. However, to do that, we're going to have to manipulate our data.
# ggplot2 wants to be able to group by a variable, so we'll have to pivot our
# tidy dataset into a longer dataset. To do this, we're going to play around
# with piping data manipulations into a ggplot() call.

# What we want to do is use the gather() function we looked at in the Tidy Data
# workshop. We can use this to "lengthen" the data, collapsing the 3 test score
# variables into 1 variable, with the effect of tripling the length of the data
# set. We can then use this pivoted data to run a density plot that's grouped by
# subject.

exams %>% 
  gather(key = "subject", value = "score", 4:6) %>%
  ggplot(aes(x=score, group = subject, fill = subject)) + 
  geom_density(alpha = 0.4) +
  theme_classic()

# Challenge:
# Create a density plot using the gapminder dataset where the data is filtered
# for the most recent year and grouped by continent, showing the distribution
# of gdpPercap in different continents.


# Multi-variable plots ----

# There are a number of different ways we could plot multiple variables at once.
# The simplest is a scatter-plot (or a dot plot).

ggplot(data = exams, aes(x = english, y = math)) +
  geom_point() +
  theme_classic()

# We can add more variables to this by adding more arguments to aesthetic.
# We can add colour and shape as variables

ggplot(data = exams, aes(x = english, y = math, colour  = history,
                         shape = test_prep_course)) +
  geom_point() +
  theme_classic()

# It's also possible for us to use different colour palettes with the 
# rcolorbrewer package.

ggplot(data = exams, aes(x = english, y = math,
                         colour = test_prep_course)) +
  geom_point() +
  theme_classic() +
  scale_colour_brewer(palette = "Set1")

# Remember that some arguments will only take categorical variable types and 
# others will only take numeric variable types. For example, we cannot use the
# shape aesthetic to show a continuous numeric variable.


# Exercise:
# Play around with different palettes in rcolourbrewer. Try mapping different 
# variables to play with sequential as well as categorical variables.


# Over-lapping data ----

# It's often the case that you have too many observations for a simple dot plot
# to capture the distribution. There are a couple of different ways you can deal
# with this depending on how large the dataset is.

# First we can 'jitter' the points. What this does is that it adds a small
# amount of random variance to the placement of a plot. This means that 
# overlapping points become visible.

ggplot(data = exams, aes(x = english, y = math,
                         colour = test_prep_course)) +
  geom_point(position = "jitter") +
  theme_classic() +
  scale_colour_brewer(palette = "Set1")

# Another approach (or one that can be used in tandom with jittering) is to edit
# the opacity of the points, so that overlapping points can be identified more
# easily.

ggplot(data = exams, aes(x = english, y = math,
                         colour = test_prep_course)) +
  geom_point(position = "jitter", alpha = 0.4) +
  theme_classic() +
  scale_colour_brewer(palette = "Set1")

# Finally, if datasets are MUCH larger and these approaches don't prove useful, 
# you can use a different type of plot, a density plot.

ggplot(data = exams, aes(x = english, y = math)) +
  geom_bin2d(bins = 50) +
  scale_fill_brewer(type = "seq", n.breaks = 20, palette = 1)

ggplot(data = exams, aes(x = english, y = math) ) +
  geom_bin2d(bins = 25) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

# Though not particularly useful here, density plots can be a very useful tool
# when dealing with very large datasets.

# Saving plots as objects ----

# It's possible to save a plot as an object so you can call it again later, or 
# for export as a png/jpeg. This is done the same way as saving any other object
# by using the assignment operator ( <- ).

# Let's see this in action
plot1 <- ggplot(data = exams, aes(x = english, y = math) ) +
  geom_bin2d(bins = 25)

# instead of the plot being constructed in the plots tab, we save it as an
# object. If we run the object, the graph will print:

plot1

# We can still add layers to our object as we could when we're layering up any
# ggplot graphic.
plot1 <- plot1 + 
  scale_fill_continuous(type = "viridis") +
  theme_bw()

plot1

# We can save this plot with the function ggsave().

ggsave(plot = plot1, filename = "hexbin_plot.png")

# This object is now saved as a plot. Note we can change it to a jpeg, or eps 
# or other file types by changing the file name.

# Adding titles and labels ----

# Let's start using a different dataset
cities <- read_csv("data/cities.csv", locale = locale(encoding="latin1"))

View(cities)


# Barchart----

# We're now going to create a bar chart using the population variables. The
# chart will show the difference in population from 2015 to 2016 by city


# you'll see in the environment that there are 63 observations (or rows) and 48
# variables. This is quite a lot of columns so we should cut it down to a more 
# manageable size to do our analysis by selecting only the columns we want to 
# use. 

# The following code uses dplyr to create a column which shows the difference 
# between the 2015 and 2016 populations. We're also only interested in cities 
# whose population has grown more than 5,000 between 2015 and 2016. 
# This new object is assigned to cities_pop

# Note the shortcut syntax ':' which means select all the columns from : to
# Also note that, because our column headers contained spaces, we need to quote
# using backticks `var name with spaces`. Another options would be to remove 
# them using functions like janitor::clean_names(cities)  

cities_pop <- cities %>%
  select(City, `Population 2013` : `Population 2016`) %>%
  mutate(diff = `Population 2016` - `Population 2015`) %>%
  filter(diff > 5000)


#plot a bar chart city population against city
ggplot(data = cities_pop, aes(x = City, y = diff))+
  geom_bar(stat = "identity")

# or we could have achieved all this by using a pipe
cities %>%
  mutate(diff = `Population 2016`-`Population 2015`) %>%
  filter(diff > 5000) %>%
  ggplot(aes(x = City, y = diff))+
  geom_bar(stat = "identity")

# We can add a title with the labs() function, adding our title after the
# 'title' argument. We can add subtitles and captions as well.
# We can label our x and y axes with the xlab() and ylab() functions.

ggplot(data = cities_pop, aes(x = City, y = diff)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(title = "2015-2016 Population changes by city",
       subtitle = "*Cities with a population change of 5,000 or more are included",
       caption = "Source: Centre for Cities.org") +
  xlab("City") +
  ylab("Difference between 2015 and 2016")

#This is starting to look better but we can still add some changes. 
# Next we'll add some colours, and reorder the bars so that
# they are descending. We can do this by using the reorder() function on our 
# City variable, telling it to reorder by our `diff` variable. We use a minus 
# sign in front of the `diff` variable to specify we want it in descending order
# The text on the axis is a bit bunched up as well so we can try fix that by
# rotating our text.
# Finally we can change the colour of our bars with the "fill" argument inside
# the geom_bar function.

my_chart <-
  ggplot(data = cities_pop, aes(x = reorder(City,-diff), y = diff)) +
  geom_bar(stat = "identity", fill = "light blue") +
  theme_classic() +
  labs(title = "2015-2016 Population changes by city",
       subtitle = "*Cities with a population change of 5,000 or more are included",
       caption = "Source: Centre for Cities.org") +
  xlab("City") +
  ylab("Difference between 2015 and 2016") +
  theme(axis.text.x = element_text(angle = -45, vjust = -1))

my_chart

#CHALLENGE 4----

# create a barchart that shows the cities that have a mean house price in
# 2017 of less than ?145,000.
# You might noice that the mean house price variable name is really long. 
# Let's change that first to make the chart easier to write using the 
# rename function in dplyr



# Extra Material ----

# 1) This coffee and coding workshop looks at how to produce a number of graphics
# in ggplot2 and export them to powerpoint slides :
# https://github.com/DataS-DHSC/coffee-and-coding/tree/master/2019-06-28 Graphs to PowerPoint

# 2) The R Graph Gallery: https://www.r-graph-gallery.com/index.html
# This index is an excellent place to see everything that ggplot2 has to offer
# It shows an example of virtually every type of graph you could name
# and has example code to produce each type. VERY useful to scrounge off 
# and speed up your workflow.

# 3) Mapping Australia's Plankton (workshop): http://www.seascapemodels.org/data/data-wrangling-spatial-course.html
# This workshop takes a few hours to go through but it's really useful to
# get to read about the delivery of a project from start to finish.
# Most of the work is done in  ggplot2, but there is a section at the end
# which discusses mapping spatial data in the raster package.

# 4) Barely R related, but the website InformationIsBeautiful is an interesting 
# look at the cutting edge of what's being done with data visualisation. 
# It's a great place to go for inspiration and to make you question what you
# could do in your own data visualisation 
# https://informationisbeautiful.net/ 


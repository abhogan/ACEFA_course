# ACEFA short course: Practical 2
# Authors: Alexandra Hogan, Oliver Eales, Freya Shearer

# R basics

###################################################################
# Welcome to the language of R. This practical is designed to be
# worked through together as a group, with instructions from
# the demonstrator.

# You can step through each line of code by pressing Ctrl+Enter on
# each line, and viewing the outputs in the Console. Feel free to
# make your own changes and check the outputs.

###################################################################
# Basic objects and operations
###################################################################

# assignment
x <- 1

# create a vector
y <- 1:10

# You can use mathematical operations
z <- x + y
z

# Functions are denoted by round brackets ()
sum(x, 10)
length(z)

# There are different types of objects in R
class(y)
a <- "hello world"
class(a)

# Combine to create new vectors
c(z, 1, 2, 3)

# Create a sequence
seq(1, 100, by = 1)

# Getting help on a function:
?c
?rnorm

# Drawing from a distribution
my_distribution <- rnorm(100)
my_distribution
plot(my_distribution)
hist(my_distribution)

max(my_distribution)
min(my_distribution)

# Try replacing rnorm() with runif()

###################################################################
# Dataframes
###################################################################

# Dataframes are important R objects that we will use throughout the course
# A dataframe is a table where each column can have a different class
# Classes that we will use are numeric, character, logical, factor
my_data_frame <- data.frame(row = 1:2,
                            country = c("AUS", "GBR"),
                            population_mill = c(25, 67),
                            health_system = c("Medicare", "NHS"),
                            southern_hemisphere = c(TRUE, FALSE)
)

my_data_frame

# We use $ notation to subset named columns in a dataframe
my_data_frame$country

############################################################################
# Packages, reading in data, and plotting
############################################################################

# one of the suites of packages useful for data analysis in R is called the tidyverse
install.packages("tidyverse") # you only need to do this once!
library(tidyverse) # you need to do this in each new R session

# read in some data
# (pertussis data from WHO (https://apps.who.int/gho/data/node.main.WHS3_43?lang=en))
pertussis <- read_csv("data/pertussis_formatted.csv")

# Some useful functions when you are reading in some data for the first time
head(pertussis)
nrow(pertussis)
ncol(pertussis)
tail(pertussis)

# Subsetting
pertussis$year

unique(pertussis$year)
max(pertussis$year)
min(pertussis$year)

unique(pertussis$Country)

# Filter a particular country (or exclude a country)
filter(pertussis, Country == "Uganda")

# Filter to a particular year or range of years
filter(pertussis, year == 2018)

filter(pertussis, year %in% c(2018, 2019, 2020))

# Select particular columns
select(pertussis, Country)
pertussis$Country

############################################################################
# More advanced concepts
############################################################################
# Mutate and summarise
y1 <- pertussis
y2 <- group_by(y1, Country)
y3 <- summarise(y2, country_sum = sum(value, na.rm = T))

# Another way to write the above using "pipes"
y4 <- pertussis %>%
  group_by(Country) %>%
  summarise(country_sum = sum(value, na.rm = T))

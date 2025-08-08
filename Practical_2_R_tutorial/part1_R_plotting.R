# ACEFA short course: Practical 2
# Authors: Alexandra Hogan, Oliver Eales, Freya Shearer

# Plotting in R

###################################################################
# In this part of the tutorial, we will prepare some basic plots
# using a package in the tidyverse called "ggplot2"
library(ggplot2)
vignette("ggplot2", package = "ggplot2")

###################################################################
# Let's start again with the pertussis data
pertussis <- read_csv("data/pertussis_formatted.csv")

# Remind ourselves of what the data look like
head(pertussis)

###################################################################
# Create a time-series plot of pertussis data in Australia

pertussis_AUS <- filter(pertussis, Country == "Australia")

p1 <- ggplot(data = pertussis_AUS, aes(x = year, y = value)) +
  geom_point(col = "darkred") +
  geom_line() +
  theme_minimal() +
  labs(x = "year", y = "cases", title = "Australia")

p1

# Could instead show as a barchart
p2 <- ggplot(data = pertussis_AUS, aes(x = year, y = value)) +
  geom_col(fill = "darkred") +
  theme_minimal() +
  labs(x = "year", y = "cases", title = "Australia")

p2

###################################################################
# What if we want to show more than a single country?

###################################################################
# Combine plots into a single figure

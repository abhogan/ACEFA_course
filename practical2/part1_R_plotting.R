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
  labs(x = "year", y = "pertussis notifications", title = "Australia")

p1

# Could instead show as a barchart
p2 <- ggplot(data = pertussis_AUS, aes(x = year, y = value)) +
  geom_col(fill = "darkred") +
  theme_minimal() +
  labs(x = "year", y = "pertussis notifications", title = "Australia")

p2

###################################################################
# What if we want to show more than a single country?
aus_pop <- 27*1e6
nz_pop <- 5*1e6

pertussis_2 <- filter(pertussis, Country %in% c("Australia", "New Zealand")) %>%
  mutate(value = if_else(Country == "Australia", value/aus_pop*1000000, value/nz_pop*1000000))

p3 <- ggplot(data = pertussis_2, aes(x = year, y = value, fill = Country)) +
  geom_col(position= "dodge") +
  theme_minimal() +
  labs(x = "year", y = "pertussis notifications per million\n(2025 population size)", title = "Australia and New Zealand") +
  theme_minimal() +
  scale_fill_manual(values = c("darkred", "pink"))

p3

###################################################################
# Combine plots into a single figure
#install.packages("patchwork")
library(patchwork)
p2+p3

###################################################################
# Another way to combine plots is using grids or facets
p4 <- ggplot(data = pertussis_2, aes(x = year, y = value)) +
  geom_col(position= "dodge") +
  facet_grid(~Country) +
  theme_minimal() +
  labs(x = "year", y = "pertussis notifications per million\n(2025 population size)", title = "Australia and New Zealand") +
  theme_minimal() +
  scale_fill_manual(values = c("darkred", "pink"))

p4


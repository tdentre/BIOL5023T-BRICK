#ASSIGNMENT 1
#TYLER
library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
blpw.all <- readRDS("~/Biology/Research Methods 2/Terns/blpw.all.RDS")

#Want birds that have been recaptured
multicapture.df <- filter(blpw.all, recap == "R")
View(multicapture.df)

#Want to take out birds with only a single weight value
recaptured.df <- multicapture.df %>%
  group_by(band, year) %>%
  filter(n() >= 2)

#Combine date columns
withdate.df <- recaptured.df %>%
  mutate(daterecap = make_date(!year, month, day))
View(withdate.df)

#Change in relative mass
relitivemass.df <- withdate.df %>%
  group_by(band) %>%
  mutate(relmass = mass - lag(mass))
View(relitivemass.df)

#NA needs to be 0
relitivemass.df[is.na(relitivemass.df)] <- 0

#This is where my other codes would go... IF I HAD ANY -.-

#Graphing
ggplot(data = relitivemass.df, mapping = aes(x = strftime(daterecap), y = relmass, group = band, color = band)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~ location, nrow = 1) + 
  theme(legend.position = "none") +
  xlab("Time of Year") +
  ylab("Mass (in grams, relative to capture date)")
  
                   
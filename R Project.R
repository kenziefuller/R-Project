rm(list = ls())
library(readr)
df <- read_csv("IPEDS_data.csv")
df <- df[complete.cases(df), ]
library(ggplot2)
# number of schools in each region
p <- qplot(Region, data = df, geom = "bar", fill = "Blue")
p
# private vs. public in each region
ptype <- qplot(Type, data = df, geom = "bar", facets = ~ Region, fill = Type)
ptype
# number of religious schools in each region
preligious <- qplot('Religious Affiliation', data = df, geom = "bar", facets = ~ Region, fill = 'Religious Affiliation')
preligious
# number of urban types in each region
purban <- qplot(Urbanization, data = df, geom = "bar", facets = ~ Region, fill = Urbanization)
purban
# tuition vs. financial aid
ptuitionaid <- qplot(Tuition, 'Percent Receiving Financial Aid', data = df, geom = "point", log = "y")
ptuitionaid
# tuition at private vs. public companies
ptuitiontype <- qplot(Type, Tuition, data = df, geom = "point")
ptuitiontype
# tuition vs. enrollment
ptuitionenrollment <- qplot(Enrollment, Tuition, data = df, geom = "point", color = "Green")
ptuitionenrollment
# map
library(choroplethrMaps)
df$State <- tolower(df$State)


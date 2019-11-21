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
purban <- qplot(Urbanization, data = df, geom = "bar", acets = ~ Region, fill = Urbanization)
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
library(choroplethr)
library(dplyr)
df$State <- tolower(df$State)
names(df)[names(df)=="State"] <- "region"

#average graduation rate per state
df <- group_by(df, region)
summ <- summarize(df, value = mean(`Graduation Rate`))
gradrates <- state_choropleth(summ, num_colors = 1)+
  scale_fill_gradient("Average Graduation Rate",low = "red", high = "blue")
gradrates

#average percent of white students per state
summ1 <- summarize(df, value = mean(`Percent White`))
diversity <- state_choropleth(summ1, num_colors = 1)+
  scale_fill_gradient("Percent White", low = "yellow", high = "blue")
diversity

#proportion of students receiving financial aid per state
library(reshape2)
summ3 <- summarize(df, value = mean(`Percent Receiving Financial Aid`))
pfaperstate <- state_choropleth(summ3, num_colors = 7, zoom = NULL, reference_map = FALSE)
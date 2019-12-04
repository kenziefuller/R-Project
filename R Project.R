# R Group Project
# Due 12/4/19
# McKenzie Fuller, Mariah Moore, Madalyn Thomas

# Clear Workspace

rm(list = ls())

# Read the file IPEDS_data.csv into a data frame

library(readr)
df <- read_csv("IPEDS_data.csv")

# Remove all records with a single NA

df <- df[complete.cases(df), ]

# Function of diversity by region

df$graduation_rate_binned <- cut(df$`Graduation Rate`, 25*(0:4))
levels(df$graduation_rate_binned)
levels(df$graduation_rate_binned) <- c("0-25%", "25-50%", "50-75%", "75-100%")


graduation.by.state <- function(state = "Washington"){
  tempdf <- subset(df, State == state)
  temp <- qplot(graduation_rate_binned, data = tempdf, geom = "bar",
                main = paste("Graduation Rates in", state, sep = " "),
                xlab = "Graduation Rate Percentage")
  temp
}
graduation.by.state()


# number of schools in each region

library(ggplot2)
p <- qplot(Region, data = df, geom = "bar", fill = I("royal blue"))
p <- p + ggtitle("Number of 4-Year Colleges per Region")
p <- p + scale_y_continuous(name = "No. of Colleges")
p

# private vs. public in each region

ptype <- qplot(Type, data = df, geom = "bar", facets = ~ Region, fill = Type)
ptype


# number of religious schools in each region

preligious <- qplot(`Religious Affiliation`, data = df, geom = "bar", facets = ~ Region, fill = `Religious Affiliation`)
preligious <- preligious + scale_fill_manual(values = c("medium blue", "steel blue1")) + ggtitle("Religious Affiliated Colleges by State")
preligious

# number of urban types in each region

purban <- qplot(Urbanization, data = df, geom = "bar", facets = ~ Region, 
                fill = Urbanization)
purban

# tuition vs. financial aid

ptuitionaid <- qplot(Tuition, `Percent Receiving Financial Aid`, data = df, 
                     geom = "point", facets = ~ Region)
ptuitionaid <- ptuitionaid + scale_x_continuous(name = "Tuition ($)", labels = comma) + ggtitle("Financial Aid Trends")
ptuitionaid

box1 <- qplot(Region, `Percent Receiving Financial Aid`, data = df, geom = "boxplot")
box1
box2 <- qplot(Region, Tuition, data = df, geom = "boxplot")
box2

# tuition at private vs. public companies

ptuitiontype <- qplot(Type, Tuition, data = df, geom = "point") + scale_y_continuous(labels = dollar)
ptuitiontype

ptuitiontype1 <- qplot(Type, data = df, geom = "bar", facets = ~ Region, fill = Type)
ptuitiontype1 <- ptuitiontype1 + scale_fill_manual(
  values = c("medium blue", "steelblue1")) + ggtitle("Types of Schools by Region")
ptuitiontype1

# tuition vs. enrollment
ptuitionenrollment <- qplot(Tuition, Enrollment, data = df, geom = "bin2d")
ptuitionenrollment <- ptuitionenrollment + ggtitle("Tuition vs. Enrollment")
ptuitionenrollment <- ptuitionenrollment + scale_x_continuous(labels = dollar) + scale_y_log10(labels = comma)
ptuitionenrollment


admitgrad <- qplot(`Percent Admitted`, `Graduation Rate`, data = df, geom = "point",
                   facets =  ~ Region, color = Tuition, alpha = I(0.5))
admitgrad <- admitgrad + scale_x_continuous(breaks = 25*(0:4)) + scale_y_continuous(name = "Graduation Rate (%)")
admitgrad <- admitgrad + scale_color_gradient(labels = dollar)
admitgrad

# diversity rates by region

white <- qplot(Region, `Percent White`, data = df, geom = "boxplot")
white <- white + ggtitle("Percent of White Students per Region")
white

# Create ChoroplethrMap of data

library(choroplethrMaps)
library(choroplethr)
library(dplyr)
library(reshape2)
dftemp <- df
dftemp$State <- tolower(dftemp$State)
names(dftemp)[names(dftemp)=="State"] <- "region"

# average graduation rate per state

dftemp <- group_by(dftemp, region)
summ <- summarize(dftemp, value = mean(`Graduation Rate`))
gradrates <- state_choropleth(summ) + scale_fill_brewer(name = "Avg. Grad Rate %")
gradrates <- gradrates + ggtitle("Average 4-Year Graduation Rates by State")
gradrates

# average percent of white students per state

summ1 <- summarize(dftemp, value = mean(`Percent White`))
diversity <- state_choropleth(summ1)+ scale_fill_brewer(name = "Percent of White Students")
diversity <- diversity + ggtitle("Average Percent of White Students by State")
diversity


# proportion of students receiving financial aid per state

summ2 <- summarize(dftemp, value = mean(`Percent Receiving Financial Aid`))
pfaperstate <- state_choropleth(summ2, num_colors = 7)
pfaperstate <- pfaperstate + scale_fill_brewer(name = "Avg. Financial Aid %")
pfaperstate <- pfaperstate + ggtitle("Average Percent of Students Receiving Financial Aid by State")
pfaperstate



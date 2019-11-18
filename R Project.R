library(readr)
df <- read_csv("IPEDS_data.csv")
df <- df[complete.cases(df), ]

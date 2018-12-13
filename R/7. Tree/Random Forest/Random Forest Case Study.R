install.packages("randomForest")

# jenitor pacakage - data cleaning - column names cleaning etc.


library(randomForest)


# Step 1: set working dir
setwd("D:/Abhijit/WK/Study/Data Science/Random Forest")
getwd()

df=read.csv("Existing Base.csv",stringsAsFactors = FALSE)
library(dplyr)
glimpse(df)
str(df)


table(df$children)
df$children = ifelse("+4","4",df$children)
  
# explainable random forest package in python
  # Tree interpreter

#h2o random forest
# its a tool 
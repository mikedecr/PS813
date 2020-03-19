# ----------------------------------------------------
#   Data Exercise 2
#   Code to get started
# ----------------------------------------------------

# load packages
library("tidyverse")
library("broom")

# run this code to install program
source("https://raw.githubusercontent.com/mikedecr/PS813/master/code/programs/create-exercise-2.R")

# run program to create data
# replace NA with your seed number
df <- 
  create_exercise_2(seed = NA) %>%
  print()


# tips: how to transform independent variables for regression
# 1) use mutate() to create new variables
# 2) use I() to transform variables WITHIN an lm() formula

# example of [2], suppose I wanted to scale X by some factor z
# lm(y ~ I(z * x), data = df)


# ----------------------------------------------------
#   Data Exercise 3
#   Code to get started
# ----------------------------------------------------

# load packages
library("tidyverse")
library("broom")

# run this code to install program
source("https://raw.githubusercontent.com/mikedecr/PS813/master/code/programs/create-exercise-3.R")

# run program to create data
# replace NA with your seed number
d3 <- 
  create_exercise_3(seed = NA) %>%
  as_tibble() %>%
  print()






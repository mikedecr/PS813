# ----------------------------------------------------
#   Data Exercise 1, PS 813
#   by Mike DeCrescenzo
# ----------------------------------------------------

# This file contains two parts. 

# First, it walks through simple R commands to complete data exercise 1.
# I have written some comments that explain what is happening and why.

# Second, at the bottom, I have written some advice for setting up your 
#   R workflow to make your life easier in the future.
#   This advice will help you with this class but it is generally 
#   good practice for doing R for any class or any research project.
#   (This is the kind of thing we talk about in PS-811 this week.)


# One last thing: A note on the code in this document
# R scripts that I (Mike) provide will use "tidyverse"-style R code,
#   consistent with the style we learn in PS-811.
# This means using code from the following packages.
# - tidyverse: this package bundles other packages, including...
#     - dplyr and tidyr for data manipulation/shaping
#     - ggplot2 for graphics
#     - helpful data modification features from stringr, forcats, purrr, etc.
# - broom: tools for model output using tidyverse-consistent methods


# ---- Data Ex. 1 -----------------------

# Run this command to download a program to create the exercise. 
source("https://raw.githubusercontent.com/mikedecr/PS813/master/code/programs/create-exercise-1.R")

# uncomment this line (delete the `#`) and enter your seed number
# my_seed <- 

# Create the exercise. Stores data in object called `d`.
d <- create_data_exercise_1(my_seed)

# view data
d




# -- get started --

library("tidyverse") # data manipulation and plotting
library("broom")     # model output




# plot the data w/ ggplot (part of tidyverse)
ggplot(d) +                       # begin a plot for dataset `d`
  aes(x = terms, y = Leg_Act) +   # declare the x and y variables
  geom_point()                    # draw points at (x, y)




# correlation 
cor(d$Leg_Act, d$terms)


# R uses a dataset$variable syntax to call a variable.
# This is because R can hold multiple datasets, 
#   some of which may have identicial variable names.
# This can be tedious but in the long-run it's really helpful.
# & luckily, tidyverse provides tons of tools for making this easier.





# estimate regression coefficients
reg <- lm(Leg_Act ~ terms, data = d)

# lm() means "linear model"
# Syntax means "Leg_Act as a function of terms" 
# We specify that these variables are in the dataset called `d`



# -- working w/ results (using {broom} package) ---

# view coefficients
reg

# or create a tidy data frame of coefficients using broom::tidy()
# shows coefs, standard errors, t-statistic, p-value
tidy(reg)


# broom::augment() returns yhat (.fitted), residuals (.resid), and more.
# Also contains the raw X and Y data
predictions <- augment(reg)

predictions





# plot residuals (y) against terms (x)
ggplot(predictions) +
  aes(x = terms, y = .resid) +
  geom_point()






# correlation of observed and predicted Y values, and squared correlation
correlation <- cor(predictions$Leg_Act, predictions$.fitted)

correlation
correlation^2

# view model R^2 (and other measures of model fit) with broom::glance()
glance(reg)

# R results sometimes print with truncated precision. 
# grab the result directly to see more
glance(reg)$r.squared

# this $ syntax works here too,  
# because glance(reg) produces a data frame w/ variables, 
# just like d is a data frame w/ variables






# ---- General R setup advice -----------------------

# Use PROJECTS with R

# 1. Create a PS-813 folder on your computer. 
#    Inside that folder, create subfolders for `data` and `code`. 
#    These will come in handy later
# 2. If you're using Rstudio, begin a "Project" for PS-813. 
#    Go to File > New Project, and then open a project in an existing 
#    directory (choose your PS-813 folder).
#    This will create a PS-813 project. A button in the top right shows 
#    which project you are working within.
#    Projects are primarily helpful for managing your directories. You can 
#    see this by running `getwd()` and noting that the current directory is
#    already set to be your PS-813 folder. 
# 3. There should now be a `.Rproj` file inside of your PS-813 folder. 
#    In the future, when you want to work on R for this course, you can
#    open this project file, and it will begin an R session with the 
#    directory already set to the appropriate location



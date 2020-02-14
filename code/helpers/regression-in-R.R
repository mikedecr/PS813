# ----------------------------------------------------
#   Guide to regression in R
# ----------------------------------------------------

# This document provides a guided previes to regression analysis with R.
# It is presented with programming style consistent with PS-811,
#   which means it is focused on a {tidyverse} workflow.

# I will discuss (1) basic mechanics of regression in R, and 
#   (2) tips that will be handy for future "Monte Carlo exercises"


# ---- load packages -----------------------

# packages contain functions that are not available in R by default.

library("tidyverse")     # data manipulation and plotting
library("broom")         # dealing with model results




# ---- load data -----------------------

# For the sake of exposition, let's use the same data as Data Exercise 1.
source("https://raw.githubusercontent.com/mikedecr/PS813/master/code/programs/create-exercise-1.R")

# we could call our data anything
d <- create_data_exercise_1(seed = 98979695)





# ---- plotting -----------------------

# The `ggplot2` plotting package is part of the "tidyverse" network.

# Here is an abbreviated introduction to how to make it work:

# We first always declare the dataset where our data come from.
# The `aes()` function declares which variables from the data are mapped to
#   aesthetic features of the plot. 
#   This primarily includes axes but could also map colors to 
#   groups of data and so on.
# `geom_*()` functions declare how you want to represent data.
#   In this case, we want to plot (x, y) coordinates as points.

ggplot(d) +
  aes(x = terms, y = Leg_Act) +
  geom_point()

# We can plot a simple line overtop the data. 
# Fit lines are "smooths" in the ggplot language. 
#   We are asking specifically for a linear model.
#   It draws a 95% conf.int for mu_i by default, 
#   but that can be suppressed/modified

ggplot(d) +
  aes(x = terms, y = Leg_Act) +
  geom_point() +
  geom_smooth(method = "lm")

# We wouldn't want to use this automatic regression line 
#   if we had a more complex model.
# Instead we would probably want to generate our own model predictions 
#   and plot them ourselves.


# ---- estimating a model -----------------------

# the lm() function estimates a linear model.
# It works by specifying a "formula" for the estimation equation.
#   y ~ x1 + x2 + ... 
# Must also declare the dataset

lm(Leg_Act ~ terms, data = d)

# won't retain the results unless we save the results in an object
ols_model <- lm(Leg_Act ~ terms, data = d)



# ---- viewing model results -----------------------

# you may have learned to view detailed model results like this:
summary(ols_model)

# Running summary() for a model prints a lot of helpful information,
#   but it isn't always easy to extract information from this object,
#   for "post-estimation" graphics and things.

# this is what the {broom} package is helpful for.

# broom::glance() returns a one-row data frame of model-level statistics
glance(ols_model)

# includes Rsq, Rsq-adjusted, sigma (residual std. error), t statistic, 
#   p.value for the whole model (F-test),
# df (number of estimated parameters), 
# df.residual (residual dfs, which is n - df), and so on


# broom::tidy() shows coefficient-level results.
# including standard errors, t-statistics, p.values, 
#   and (optionally) confidence intervals for each estimate
tidy(ols_model, conf.int = TRUE)



# ---- generating predicted values, residuals... -----------------------

# broom::augment() generates predicted values,
#   and other "observation-level" model results

augment(ols_model)

# .fitted = mu_i
# .se.fit = sqrt(var(mu_i))
# .resid = residual (y_i - mu_i)



# you can use augment() to generate predictions for NEW observations
# You have to create a dataset containing variables with the same names

# Make a data frame from scratch using the `tibble()` function.
# I will call it `prediction_data`.
# It contains a variable called `terms`,
#   which is a sequence of values from 1 to 20.

prediction_data <- tibble(terms = seq(1, 20, 1))

# let's see what we made
prediction_data

# Generate predicted values for new observations
#   with the `newdata = ...` argument in augment()
augment(ols_model, newdata = prediction_data)


# ---- confidence intervals for predicted values -----------------------

# one feature of broom::augment() is that 
#   it doesn't give you a CI for your predicted value mu_i
# But it DOES give you the standard error of mu_i, 
#   which is sqrt(var(mu_i)).
# You can use this to generate a confidence interval,
#   as long as you supply the appropriate *critical value* 


# what is the critical value for this model?
# For a 95% interval, we find t-value with only 2.5% probability above,
#   using the residual df value from the model

# what are the residual dfs? (saving this value to use later)
ols_dfs <- glance(ols_model)$df.residual
ols_dfs

# what is the t value (quantile) that leaves 2.5% prob in the upper tail?
# (again, saving it)
critical_t <- qt(p = 0.025, df = glance(ols_model)$df.residual)
critical_t



# Create new variables `conf.low` and `conf.high` using mutate()
# Multiply critical value by standard error to get MOE
# Get confidence bounds as prediction +/- MOE

new_predictions <- ols_model %>%
  augment(newdata = prediction_data) %>%
  mutate(
    conf.low = .fitted - (critical_t * .se.fit),
    conf.high = .fitted + (critical_t * .se.fit)
  ) %>%
  print()


# you may wondering: wtf is this code doing? 
# I will tell you:



# ---- debriefer on the %>% syntax -----------------------

# The tidyverse introduces a new way to write functions
#   that you may not have seen before.
# BUT it is extremely convenient, which I will explain here.

# suppose I have a function f(), it takes data x.
# I could call it like f(x)

# for instance, taking the mean of some variable z
z <- 1:10
mean(z)

# I can also write x %>% f(),
# which means, "take x and pass it to function f"
z %>% mean()

# This is called the PIPE OPERATOR: %>%
# It pushes data forward from one function to the next.
#   (for this reason it is also called a "forward pipe")
# The pipe assumes that the first argument of the next function 
#   will be the data, so you can omit the first argument! 
#   (We will relax this assumption later.)

# You may be asking, okay so what?
# This is valuable because of what happens when you chain things together.
# instead of writing i(h(g(f(x))))...
#   I can write x %>% f() %>% g() %>% h() %>% i()
# This allows me to write a series of operations LINEARLY,
#   instead of "inside out," as order of operations normally requires.

# pseudo-example: https://twitter.com/dmi3k/status/1191824875842879489

# let's review this code. Read the pipe operator as saying "and then..."

new_predictions <- ols_model %>%
  augment(newdata = prediction_data) %>%
  mutate(
    conf.low = .fitted - (critical_t * .se.fit),
    conf.high = .fitted + (critical_t * .se.fit)
  ) %>%
  print()

# it says:
# Start with OLS model, then...
# use it to generate predictions for `prediction_data`, then...
# create new variables (conf.low = ..., conf.high = ...), then...
# print the results

# By printing at the end of a "pipe chain", 
#   you can view the results at the same time as you save them



# What if the data shouldn't be the first argument? 
# You can use a period (.) to stand in for the data
#   when the data shouldn't be the first argument.

# example, in lm(), the data aren't the first argument
lm(Leg_Act ~ terms, data = d)

# if I pipe data into lm(), I can specify that the data go somewhere else
d %>% lm(Leg_Act ~ terms, data = .)


# to further illustrate this point, 
#   imagine that I create CIs as above,
#   but instead of starting with ols_model, I start with `prediction_data`
# I would need to specify that the data go in the right place in augment():

new_predictions <- prediction_data %>% 
  augment(ols_model, newdata = .) %>%
  mutate(
    conf.low = .fitted - (critical_t * .se.fit),
    conf.high = .fitted + (critical_t * .se.fit)
  ) %>%
  print()



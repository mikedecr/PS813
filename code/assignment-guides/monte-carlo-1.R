# ----------------------------------------------------
#   Guide to Monte Carlo Exercise #1
#   This will ONLY get you started!!! 
#   Follow assignment instructions & modify code accordingly.
# ----------------------------------------------------

# This script describes 3 things
# 1. How to run the monte carlo function (BASICS)
# 2. How to stack data from multiple monte carlo runs (manually)
# 3. How to stack data from multiple monte carlo runs (efficiently)

# I suggest you read and test this script all the way to the bottom
# BEFORE deciding on the approach you want to take.


# you need to load these packages
library("tidyverse") # data management and graphics
library("broom")     # model output (needed for corerr())


# supply your seed number with function set.seed()
set.seed(2002181901)

# Run the following command.
# This downloads the `corerr()` function into your R session, 
#   which you can use to make monte carlo datasets.
source("https://raw.githubusercontent.com/mikedecr/PS813/master/code/programs/monte-carlo_1_corerr.R")

# this command shows available R objects. 
# You will see "corerr" if you ran the above.
ls()


# The corerr() program works like Dave's example.
# when you call corerr(), replace each `...` with a number.
corerr(
  sample_size = ... ,
  intercept = ... , 
  slope = ... ,
  sd_predictor = ... , 
  sd_disturbance = ... ,
  cor_disturbance = ... 
)

# As always, save your results in an object so you can get them back later.


# tips:

# - save parameter values (slope, intercept...) as objects ahead of time, 
#   and pass objects to corerr() instead of hand-typing numbers each time
#   (also makes analysis/plotting easier later)

# - If you run corerr() with different assumptions, 
#   you can stack the output datasets using bind_rows().
#     bind_rows(dataset1, dataset2, etc)
#   This can make it easier to plot results from multiple Monte Carlos.






# ---- Combining Monte Carlos by stacking data manually --------------

# Suppose I have two monte carlo samples.
# one has zero error correlation, one has .1 correlation
mc_cor00 <- corerr(
  sample_size = 100 ,
  intercept = 0 , 
  slope = 1 ,
  sd_predictor = 10 , 
  sd_disturbance = 5 ,
  cor_disturbance = 0 
)

mc_cor10 <- corerr(
  sample_size = 100 ,
  intercept = 0 , 
  slope = 1 ,
  sd_predictor = 10 , 
  sd_disturbance = 5 ,
  cor_disturbance = .1
)

# combine these with bind_rows()
bind_rows(mc_cor00, mc_cor10)

# In order to tell your datasets apart, you need to label them. 
# - either add a variable to a dataset before binding, or...
# - label the data during the binding process

# here is an example of the latter:
bind_rows(
  "0" = mc_cor00,
  "0.1" = mc_cor10,
  .id = "cor_disturbance"
)

# this setup adds an ID variable called "cor_disturbance"
#   that gets "0" for the data from one dataset
#   and gets ".1" for the other dataset
# Your labels must be strings: "label_name" = dataset_object
# (You can convert strings to numbers later, 
#  with as.numeric() or parse_number().)







# ---- More efficient setup but a little more advanced ---------------

# here is an advanced approach, but it is fast and needs less code.
# It will work if you adapt the code to your parameters appropriately.

# some setup:
# save vectors that contain your assumptions
# (You will want to vary the error correlation and some other thing)
true_alpha <- c(0, 1, 5, 10)
true_sigma <- c(1, 2, 5, 10)

# make a dataset where every row is a combination of alpha and sigma
cross_data <- 
  crossing(true_alpha, true_sigma) %>%
  print()


# so far, that was just raw material for the real deal.
# Use this data frame as a skeleton for estimating many monte carlos



# running corerr() for every "row" of parameters.
# This is done using the map() family of functions.

# map2() will apply function across 2 input variables.
# - Notice I call corerr() with .x and .y as stand-ins.
# - You must plug in .x and .y for the variables you are varying.
monte_carlo_stack <- cross_data %>%
  mutate(
    monte_carlo = map2(
      .x = true_alpha, .y = true_sigma, .f = ~ {
        corerr(
          sample_size = 100,
          intercept = .x , 
          slope = 1 ,
          sd_predictor = 10, 
          sd_disturbance = .y,
          cor_disturbance = 0
        )
      }
    )
  ) %>%
  print()


# this makes a "nested data frame" 
# `monte_carlo` is a variable in this data frame, 
#   but it contains a bunch of data frames within it. (whoa dude...)
# You need to unnest it to interact with the contents of each data frame
monte_carlo_full <- monte_carlo_stack %>%
  unnest(monte_carlo) %>%
  print()

# the estimates from each simulation run are still labeled 
#   with their true_alpha and true_sigma values.
# But instead of manually running every monte carlo and bind_rows(),
#   I made it work at scale.

# I can use this combined data to plot things from every MC in one plot...
ggplot(monte_carlo_full) +
  aes(x = slope) +
  geom_histogram(bins = 100) +
  facet_grid(true_alpha ~ true_sigma)




# or I could summarize each monte carlo
#   by grouping the data and then calculating summary stats
#   (which you can use to calculate bias, RMSE or whatever you'd like)
monte_carlo_full %>%
  group_by(true_alpha, true_sigma) %>%
  summarize(
    mean_intercept = mean(intercept),
    mean_slope = mean(slope)
  ) 

# this would let you get other stats to plot



# If you don't want all of your datasets stacked,
#   unstack by filtering.
# This example: keep only rows where alpha was 0 and sigma was 1
filter(monte_carlo_full, true_alpha == 0 & true_sigma == 1)



# here's one last fun graphics tip for labelling things.
# Suppose I had a variable true_alpha
true_alpha

# I can plug this variable into a string with str_glue(), 
#   putting the names of R objects into curly braces {}
str_glue("Alpha = {true_alpha}")


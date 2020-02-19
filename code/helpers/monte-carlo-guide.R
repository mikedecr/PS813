# ----------------------------------------------------
#   Guide to Monte Carlo routines in R
# ----------------------------------------------------

# You should always open R from the ".Rproj" file in your PS-813 folder.
# If you don't know what that means, check the Data Exercise 1 script!

# If you did this right, 
#   your R session will start with your "working directory" 
#   automatically set to your PS-813 folder.
# check that this is true with getwd() (a.k.a., "get working directory")
getwd()


# ---- packages -----------------------

# packages come first
library("tidyverse")
library("broom")


# ---- overview -----------------------

# You will soon be doing coding exercises that demonstrate what happens
#   when models do/don't meet assumptions.
# However, in the real world, you only have one dataset, 
#   so it is difficult to visualize the consequences of assumptions.
# These exercise use a LARGE NUMBER of simulated datasets
#   in order to get a broader picture about expected model behavior.

# Using large numbers of simulations to display statistical properties
#   is often called a "Monte Carlo" routine. 

# In this script, I will discuss how you can simulate a dataset in R.
# AND THEN I will discuss how you can scale it up for Monte Carlo analysis.



# ---- Simulating a single sample -----------------------

# Suppose the "true model" is y = alpha + beta*x + epsilon, 
#   where alpha = 1, beta = 0, and sigma^2 = 1.
# I will generate 100 X and Y data points according to this model.

true_alpha <- 1
true_beta <- 0
true_sigma <- 1

sample_size <- 100

# MAKE A DATASET:
# IV is a fixed sequence of values from 0 to 5
# epsilon is normal(0, sigma)
# DV = alpha + beta(x) + epsilon
one_draw <- 
  tibble(
    IV = seq(from = 0, to = 5, length.out = sample_size),
    epsilon = rnorm(n = sample_size, mean = 0, sd = true_sigma),
    DV = true_alpha + (IV * true_beta) + epsilon
  ) %>%
  print()

# plot data
ggplot(one_draw) +
  aes(x = IV, y = DV) +
  geom_point() 

# estimate the model
one_model <- lm(DV ~ IV, data = one_draw)

# view coefficient estimates
tidy(one_model, conf.int = TRUE)


# how do I access alpha and beta values?
coef(one_model)
coef(one_model)["(Intercept)"]
coef(one_model)["IV"]

# use broom::tidy() for a dataframe-oriented approach
#   (which has benefits for future plotting)
tidy(one_model) %>% 
  filter(term == "(Intercept)") %>%
  pull(estimate)




# ----------------------------------------------------
#   Monte Carlo analysis
# ----------------------------------------------------

# I will describe two approaches to Monte Carlo routines.
# 1. "for" loops
# 2. "apply" functions


# ---- Guide to Loops -----------------------

# One way to do Monte Carlo simulation is "for loops"

# the basic idea...
# for each value in a series, repeat an operation
hypothetical_sequence <- c(1, 2, 3, 4)

for (n in hypothetical_sequence) {
  print(n)
}


# it repeats the operation, each time where `n` takes a different value

# another example where we use the for loop with indexing
# make an empty numeric variable v, which will hold our results
v <- numeric()
v

# loop over values of i
# assign a result to the i'th value of v
for (i in 1:10) {
  v[i] <- i * 2
}

# what is v now?
v

# it's actually smarter to loop over indices with seq_along().
# Make an empty container of arbitrary size
v <- rep(NA, 10)
v

# then loop over the length of this variable
for (i in seq_along(v)) {
  v[i] <- i * 2
}



# ---- Monte Carlo, using loops -----------------------

# we will repeat the simulation some arbitrary number of times M.
# Best to save this number as a variable ahead of time.
M_simulations <- 500


# create a container for all M results:
#   data frame of empty coefficient estimates
#   (NA = missing number, but repeat it however many times)

results <- 
  tibble(
    iteration = seq(from = 1, to = M_simulations, by = 1),
    a_hat = rep(NA, M_simulations),
    b_hat = rep(NA, M_simulations)
  ) %>%
  print()




# now create a loop.
# for each iteration n = {1, 2, ...}, 
#   - simulate a dataset from the true model, 
#   - estimate OLS regression for the data
#   - store coefficient estimates in a_hat[n] and b_hat[]


# ---- begin loop -----------------------
for (m in 1:M_simulations) {

  # make a dataset, estimate model
  sim_data <- tibble(
    IV = seq(0, 5, length.out = sample_size),
    epsilon = rnorm(n = sample_size, mean = 0, sd = true_sigma),
    DV = true_alpha + (IV * true_beta) + epsilon
  )

  model <- lm(DV ~ IV, data = sim_data) 

  # this is the iteration-specific stuff
  # save estimates in the n'th value of these variables
  results$a_hat[m] <- tidy(model) %>%
    filter(term == "(Intercept)") %>%
    pull(estimate)

  results$b_hat[m] <- tidy(model) %>%
    filter(term == "IV") %>% 
    pull(estimate)

  # print the value of n to track our progress
  # ( not necessary )
  print(m)

}
# ---- end loop -----------------------


# check out the results

# histograms take an X variable, calculate bar height (y) automatically
# also add vertical lines
ggplot(results) +
  aes(x = a_hat) +
  geom_histogram() +
  geom_vline(xintercept = true_alpha) +
  geom_vline(aes(xintercept = mean(a_hat)), color = "red")

ggplot(results) +
  aes(x = b_hat) +
  geom_histogram() +
  geom_vline(xintercept = true_beta) +
  geom_vline(aes(xintercept = mean(b_hat)), color = "red")

# here's that negative covariance we talked about a few weeks ago
ggplot(results) +
  aes(x = a_hat, y = b_hat) +
  geom_point()



# ---- Guide to "apply" functions -----------------------

# Loops in R are known for being slow (compared to other languages).
# And in cases where iteration `m` doesn't depend on `m - 1`,
#   they can be considered to "poor practice"

# "apply" functions are a fast alternative to lists in situations when
#   result `m` does not depend on `m - 1`


# The function we will use is lapply(), 
#   which applies a function to every element in a list.

# What is a list? An object containing other objects of arbitrary type.
list(1:5, "a", FALSE, mtcars[1:5, ])

# list element [[1]] is a numeric vector
# list element [[2]] is a length-1 character vector
# list element [[3]] is a length-1 logical vector
# list element [[4]] is a slice of a data frame


# How does this help us? 
# You can do monte carlos by making a LIST of datasets,
#   and then estimating the model in every dataset.
# This runs super fast and (in my opinion) is easier to write than loops


# Let's demonstrate.
# Suppose we had a list of numeric vectors. 
# (Don't worry about how I make this)
test_list <- vector(mode = "list", length = 5) %>%
  lapply(function(x) round(runif(n = 5, min = 0, max = 10))) %>%
  print()


# use lapply() to find the mean of every element of the list

# lapply(list_name, function_name)
lapply(test_list, mean)



# ---- Monte Carlos with lists and lapply() -----------------------

# Now we will demonstrate with lapply()

# first, make a simple list put datasets into.
empty_list <- 
  as.list(1:M_simulations) %>%
  print()


# To this list, apply a function that makes a dataset.
# format: lapply(X = list_name, FUN = function_name)
#         and then you can supply additional function arguments
# (note how much faster this runs than the loop)
data_list <- lapply(
  X = empty_list,
  FUN = tibble,
    IV = seq(0, 5, length.out = sample_size),
    epsilon = rnorm(sample_size, mean = 0, sd = true_sigma),
    DV = true_alpha + (IV * true_beta) + epsilon
)

# I don't want to print a lot of datasets, 
# so use head() to print the first 6
head(data_list)

# estimate model for each dataset
# This uses a different function format, called an "anonymous function"
# (you basically define a function for using within lapply)
# Makes it easier to put the data somewhere other than 
#   the first arg or the function
model_list <- lapply(
  X = data_list,
  FUN = function(d) lm(DV ~ IV, data = d)
)

head(model_list)


# apply tidy() for every model in the list
tidy_list <- lapply(model_list, tidy)

head(tidy_list)


# now use bind_rows (part of tidyverse) to stack the data frames
results <- bind_rows(tidy_list) %>%
  print()

# now plot as you wish
results %>%
  filter(term == "IV") %>%
  ggplot() +
  aes(x = estimate) +
  geom_histogram() +
  geom_vline(aes(xintercept = mean(estimate)))



# you could use an anonymous function to do all of this at one time!
all_at_once_list <- lapply(
  X = empty_list,
  FUN = function(x) {
    tibble(
      IV = seq(0, 5, length.out = sample_size),
      epsilon = rnorm(sample_size, mean = 0, sd = true_sigma),
      DV = true_alpha + (IV * true_beta) + epsilon
    ) %>%
    lm(DV ~ IV, data = .) %>%
    tidy()
  }
)


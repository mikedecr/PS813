# ----------------------------------------------------
#   Guide to Monte Carlo routines in R
# ----------------------------------------------------

library("tidyverse")
library("broom")


# You will soon be doing coding exercises that demonstrate what happens
#   when models do/don't meet assumptions.
# In the real world, you only have one dataset, 
#   so it is difficult to visualize the consequences of assumptions.
# So these exercise use a LARGE NUMBER of simulated datasets
#   in order to get a broader picture about expected model behavior.

# Using large numbers of simulations to display statistical approximations
#   is often called a "Monte Carlo" routine. 

# I will discuss how you can simulate a dataset in R.
# AND THEN I will discuss how you can scale it up for Monte Carlo analysis.



# ---- Simulating a single sample -----------------------

# Suppose the "true model" is y = alpha + beta*x + epsilon, 
#   where alpha = 1, beta = 0, and sigma^2 = 1.
# I will generate 100 X and Y data points according to this model.

true_alpha <- 1
true_beta <- 0
true_sigma <- 1

sample_size <- 100

# make a dataset
# IV is a fixed sequence of values from 0 to 5
# epsilon is normal(0, sigma)
# DV = alpha + beta(x) + epsilon
one_draw <- 
  tibble(
    IV = seq(0, 5, length.out = sample_size),
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



# ---- Guide to loops -----------------------

# One way to do Monte Carlo simulation is "for loops"

# the basic idea...
# for each value in a series, repeat an operation

for (n in c(1, 2, 3, 4)) {
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



# ---- Monte Carlo, using loops -----------------------

# number of times to iterate
n_simulations <- 500


# create a container for results:
#   data frame of empty coefficient estimates
#   (NA = missing number, but repeat it however many times)

results <- 
  tibble(
    iteration = 1:n_simulations,
    a_hat = as.numeric(NA) %>% rep(n_simulations),
    b_hat = as.numeric(NA) %>% rep(n_simulations)
  ) %>%
  print()




# now create a loop.
# for each iteration n = {1, 2, ...}, 
#   - simulate a dataset from the true model, 
#   - estimate OLS regression for the data
#   - store coefficient estimates in a_hat[n] and b_hat[]


# ---- begin loop -----------------------
for (n in 1:n_simulations) {

  # make a dataset, estimate model
  sim_data <- tibble(
    IV = seq(0, 5, length.out = sample_size),
    epsilon = rnorm(n = sample_size, mean = 0, sd = true_sigma),
    DV = true_alpha + (IV * true_beta) + epsilon
  )

  model <- lm(DV ~ IV, data = sim_data) 

  # this is the iteration-specific stuff
  # save estimates in the n'th value of these variables
  results$a_hat[n] <- tidy(model) %>%
    filter(term == "(Intercept)") %>%
    pull(estimate)

  results$b_hat[n] <- tidy(model) %>%
    filter(term == "IV") %>% 
    pull(estimate)

  # print the value of n to track our progress
  # ( not necessary )
  print(n)

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



# ---- Loops work, but there are other ways -----------------------

# Loops in R are known for being slow (compared to other languages).
# And in cases where iteration `m` doesn't depend on `m - 1`,
#   they can be considered to "poor practice"

# If you're feeling comfortable in R, I encourage using lapply(),
#   which applies a function over the elements in a list.

# What is a list? An object containing other objects of arbitrary type.
list(1, "a", sd)

# You can do monte carlos by making a LIST of datasets,
# and then applying functions to that list


# first, make a simple list put datasets into
# I will use print() to display results while assigning objects
empty_list <- 
  as.list(1:n_simulations) %>%
  print()

# apply a function to this list
# where the function makes a dataset
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


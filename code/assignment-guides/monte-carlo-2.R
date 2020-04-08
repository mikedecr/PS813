# ----------------------------------------------------
#   Guide to Monte Carlo Exercise #2
#   This will ONLY get you started!!! 
#   Follow assignment instructions & modify code accordingly.
# ----------------------------------------------------

library("tidyverse")
library("broom")


# get monte carlo program from online
source("https://raw.githubusercontent.com/mikedecr/PS813/master/code/programs/monte-carlo_2_errvar.R")

# demo with one set of arguments
errvar(
  sample_size = 1000,
  intercept = 0,
  slope = 1,
  sd_v = 1,
  sd_disturbance = 5,
  cor_wx = 0.95,
  n_simulations = 1000,
  seed = 32123
) %>%
  print()


# demo: mapping errvar() over multiple input
# like last time...
# - save parameters that you want to vary ahead of time
# - create a data frame representing every combination of params
# - apply errvar() to all param combinations
MCs <- 
  crossing(
    alpha = c(0, 1, 2),
    sims = c(100, 200, 500)
  ) %>%
  mutate(
    monte_carlo = map2(
      .x = alpha,
      .y = sims,
      .f = ~ errvar(
        sample_size = 1000,
        intercept = .x, 
        slope = 1, 
        sd_v = 0.25, 
        sd_disturbance = 5, 
        cor_wx = .5, 
        n_simulations = .y, 
        seed = 32123
      )
    ) 
  ) %>%
  print()


# to expand your coefficient estimates, 
# unnest() the results
MCs %>%
  unnest(monte_carlo)



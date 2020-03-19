# ----------------------------------------------------
#   Monte Carlo 1 - "Solutions"
# ----------------------------------------------------

library("magrittr")
library("tidyverse") # data management and graphics
library("broom")     # model output (needed for corerr())


# supply your seed number with function set.seed()
set.seed(1110011)

# Run the following command.
# This downloads the `corerr()` function into your R session, 
#   which you can use to make monte carlo datasets.
source("https://raw.githubusercontent.com/mikedecr/PS813/master/code/programs/cor-err.R")



# fix these parameters
true_alpha <- 5
true_beta <- 1

# vary these parameters
error_correlation <- c(-.3, 0, .1, .3, .6, .9)
sd_disturbance <- c(.5, 1, 2, 5)



# make a data frame to hold results
cross_data <- 
  crossing(error_correlation, sd_disturbance) %>%
  print()

# map() the corerr program across all conditions
MCs_stack <- cross_data %>%
  mutate(
    monte_carlo = map2(
      .x = error_correlation, 
      .y = sd_disturbance, 
      .f = ~ corerr(
               sample_size = 100, 
               intercept = true_alpha, 
               slope = true_beta, 
               sd_predictor = 5, 
               sd_disturbance = .y, 
               cor_disturbance = .x 
             )
    )
  ) %>%
  print()



# unnest each simulation
MCs <- MCs_stack %>%
  unnest(monte_carlo) %>%
  print()


# plot histogram of slopes vs. true
ggplot(MCs) +
  aes(x = slope - true_beta) +
  facet_grid(sd_disturbance ~ error_correlation) +
  geom_histogram(binwidth = .01, fill = "darkcyan") +
  geom_vline(xintercept = true_beta) +
  theme_minimal()


# calculate empirical estimates of variance, bias, and RMSE
MC_stats <- MCs %>%
  group_by(error_correlation, sd_disturbance) %>%
  summarize(
    variance = var(slope),
    est_bias = mean(slope - true_beta),
    rmse = sqrt(est_bias^2 + variance)
  ) %>%
  gather(key = stat, value = value, variance, est_bias, rmse) %>%
  mutate(
    stat = fct_relevel(stat, "est_bias", "variance", "rmse")
  ) %>%
  print()


# plot summaries of these statistics
ggplot(MC_stats) +
  aes(x = error_correlation, y = value, color = as.factor(sd_disturbance)) +
  facet_grid(. ~ stat) +
  geom_vline(xintercept = 0, color = "gray50") +
  geom_hline(yintercept = 0, color = "gray50") +
  geom_line() +
  geom_point() +
  theme_minimal() +
  scale_color_viridis_d(end = 0.7)






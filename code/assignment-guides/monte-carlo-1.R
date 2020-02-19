# ----------------------------------------------------
#   Guide to Monte Carlo Exercise #1
#   This will ONLY get you started!!! 
#   Follow assignment instructions & modify code accordingly.
# ----------------------------------------------------



# you need to load these packages
library("tidyverse")
library("broom")


# supply your seed number with function set.seed()
set.seed(2002181901)

# Run the following command.
# This downloads the `corerr()` function.
source("https://raw.githubusercontent.com/mikedecr/PS813/master/code/programs/cor-err.R")


# this command shows available R objects. 
# Should show "corerr"
ls()


# when you call corerr(), replace each `...` with a number
mc_data <- corerr(
  sample_size = ... ,
  intercept = ... , 
  slope = ... ,
  sd_predictor = ... , 
  sd_disturbance = ... ,
  cor_disturbance = ... 
)

# you can then view results as a histogram
ggplot(mc_data) +
  aes(slope) +
  geom_histogram()

# tips:
# - you can display vertical lines for true values with geom_vline(...)
# - save parameter values as objects for easier plotting later
# - If you run corerr() with different assumptions, 
#   you can stack the output datasets using bind_rows().
#     bind_rows(dataset1, dataset2, etc)
#   This will make it easier to plot results from multiple Monte Carlos.

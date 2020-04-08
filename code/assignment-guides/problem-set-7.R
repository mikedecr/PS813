# ----------------------------------------------------
#   Problem Set 7: Heteroskedasticity
#   Guide for R
#   by Michael DeCrescenzo (Spring 2020)
# ----------------------------------------------------

# NOTES:
# This problem set contains code work as well as written work.
# You may want to do this one in R Markdown if you're comfortable with that.
# Otherwise you can upload photos using the app I described in an 
#   earlier class announcement about online submissions! 


# ---- packages -----------------------


library("here")      # file paths
library("tidyverse") # data manipulation/plotting
library("broom")     # model output

# new packages for this script
library("lmtest")    # contains many common model diagnostic tests
library("estimatr")  # OLS w/ robust std. errors

# also consider:
# install.packages("prediction") # more model prediction functions


# ---- read data -----------------------

# read in the data
het <- haven::read_dta() # add file path to data


# ---- 1a -----------------------

# you should be able to do this part...
simple_model <- lm(y ~ x1 + x2 + x3, data = het)



# ---- 1b: heteroskedasticity tests -----------------------

# these functions are from the {lmtest} package.
# READ CAREFULLY.
# I have set up these examples so that they DO NOT do
#   what the problem set asks you to do by default.
# This means you gotta MAKE THEM WORK the you need

# Goldfield-Quandt test partitions the dataset 
#   by removing some middle fraction of the data,
#   estimating the model in the two remaining fractions
#   and comparing their residual variances.
# The way to remove a "middle fraction" is that the data must be SORTED
#   according to predictor you want to examine ahead of time. 
#   (I do this with arrange() below. Note which predictor I use.)
# `fraction` argument controls how much of the middle of the data to drop.
#   MUST CHOOSE what this fraction will be.
# `alternative` argument specifies the alternative hypothesis.
#   (two-sided or one-sided tests)
# see help(gqtest) for more

gqtest(
  formula = y ~ x1 + x2 + x3, 
  data = arrange(het, x2),
  fraction = 0.1,
  alternative = c("two.sided")
)

# Breusch-Pagan-Godfrey test regresses squared residuals on predictors.
# `varformula` argument takes a right-hand-side formula of predictors 
#   (don't include y)
#   If not specified, defaults to the full model formula.
# Notice how results change if you speculate that heteroskedasticity
#   comes from x1, vs x2, vs x3, vs all predictors 
# see help(bptest) for more

# w/ this example I show you how to recycle formula and data
#   from the model object itself.
# supposing I had a model called "my_model" ...

bptest(
  formula = simple_model$terms,
  data = simple_model$model,
  varformula = ~ x3
)





# ---- 1d (c is missing?) -----------------------

# estimtr::lm_robust() estimates OLS models with robust std. errors.
# https://declaredesign.org/r/estimatr/articles/getting-started.html#lm_robust

robust_model <- lm_robust(y ~ x1 + x2 + x3, data = het)

# broom::tidy() and broom::glance() work for lm_robust models
# augment does not, but predict() does,
# and so does prediction::prediction(), which I prefer over predict().

# An exampe of prediction::prediction() for the first model.
# You can also specify interval = "confidence" or "prediction"
# convert to tibble to get pretty output
prediction::prediction(
  simple_model, 
  interval = "confidence"
) %>%
  as_tibble()


# ---- 2 is an analytical exercise -----------------------



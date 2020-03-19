# ----------------------------------------------------
#   Problem Set 6 help in R: 
#   
#   Contains a few different things:
#   - Problem 1: do this without the computer
#   - Problem 2: R translation of Stata code contained below 
#                (plus some extra things to consider for R)
#   - Problem 3: Derive by hand 
#                (but I do introduce a workhorse MLE regression f() in R)
#   - Problem 4: I show how to:
#     > get the data into R
#     > find Cook's D for each obs
#     > replace a values for parts 4.2 and 4.3
# ----------------------------------------------------


library("tidyverse")
library("broom")




# ---- problem 1 -----------------------

# do by hand








# ---- problem 2 -----------------------

# here is a translation of Stata matrix commands

A <- matrix(
  data = c(1, 5, 
           1, 7, 
           1, 3, 
           1, 4),
  byrow = TRUE,
  ncol = 2
)

A



B <- matrix(c(2, 8), nrow = 1)

B



AT <- t(A)

AT



BT <- t(B)

BT



ATA <- AT %*% A

ATA



ABT <- A %*% BT

ABT


DetATA <- det(ATA)

DetATA



InvATA <- solve(ATA)

InvATA



TrcATA <- sum(diag(ATA))

TrcATA


Qad <- B %*% InvATA %*% BT

Qad


# the last command...this is how you would delete all saved objects in R
# I don't recommend that you run it though...
# you want the objects what's below
rm(list = ls())

# translation:
# - rm() function removes objects
# - ls() is a character vector of all existing object names 
# - q.e.d. so you're telling R to remove all existing objects


# ---- problem 2: extra notes-----------------------

# some additional notes about matrices in R (from Mike DeCrescenzo)


B            # a 1x2 matrix
as.vector(B) # coercing to a vector: in this case a 2-length vector



# On the difference between * and %*% 

as.vector(B) * as.vector(B)   # "element-wise" product
as.vector(B) %*% as.vector(B) # dot product (matrix multiplication)



# why doesn't this work? ( hint: dim(B) )
B %*% B

# a web page describing other helpful matrix functions:
# https://www.statmethods.net/advstats/matrix.html



# ---- problem 3 -----------------------

# This is a by-hand problem.

# BUT if you want to do regressions using MLE, 
#   chances are you want to estimate a "generalized linear model" ("glm")

# If a linear model is 
#   Y = Xβ + ε, 
#   and ε ~ Normal(0, σ)
# then a generalized linear model is 
#   Y = g(Xβ) + ε, 
#   where ε ~ D(??)
# g() is called a "link function"
# and D() is some probability distribution

# a linear model with Normal errors is a SPECIAL CASE of a GLM,
#   where the link function g(x) = 1*x 
#   (aka the "identity function" or "identity link")
#   and the distribution of errors is Normal/Gaussian

# estimate using glm() instead of lm()
#   needs you to specify the error distribution (family = ...) 
#   and link function (link = ...)

glm(y ~ x, data = df, family = gaussian(link = "identity"))

# we will see this when we do logit, probit, poisson...

# more generally, if you're ever in a situation where you need an MLE
#   for some non-standard likelihood function,
#   you can use the {maxLik} package




# ---- problem 4 -----------------------

# 4.1: dealing w/ outliers

# makes a data frame, row-by-row
df <- tribble(
  ~ x, ~ y, 
  -5,  -1, 
  -5,   0, 
  -5,   1, 
  -4,  -1, 
  -4,   0, 
  -4,   1, 
  -3,  -1, 
  -3,   0, 
  -3,   1, 
  -2,  -1, 
  -2,   0, 
  -2,   1, 
  -1,  -1, 
  -1,   0, 
  -1,   1,
   0,  -1, 
   0,   0, 
   0,   1, 
   1,  -1, 
   1,   0, 
   1,   1, 
   2,  -1, 
   2,   0, 
   2,   1, 
   3,  -1, 
   3,   0, 
   3,   1, 
   4,  -1, 
   4,   0, 
   4,   1, 
   5,  -1, 
   5,   0, 
   5,   1, 
  20,   0
) %>%
  print()


# Estimate a model with lm() function.
# Cook's D is a variable generated from augment().
# For help with regression, 
#   - see "regression in R" file that sent weeks ago, 
#   - or 811 notes


# 4.2: replacing the 34'th y value

# two ways you can do this:
# one: assign a new value to the 34th element of y
df$y[34] <- 50

# two: use case_when() to change the y value of 34th row 
df <- df %>%
  mutate(
    y = case_when(row_number() == 34 ~ 50, 
                  TRUE ~ y)
  ) %>%
  print()

# now repeat 4.1 using the altered data




# 4.3: adding random noise

# the stata does this:
#   1. start with y
#   2. generate uniform random noise between (0, 1) and multiply by 60
#      (this creates uniform noise between (0, 60))
#   3. subtract 30

# This results in uniform random noise between (-30, 30).
# We can do this directly in R.
# - runif() generates draws from uniform distribution
# - n = how many draws
# - min = ..., max = ...: what's the inverval

df <- df %>%
  mutate(
    y = y + runif(n = n(), min = -30, max = 30)
  ) %>%
  print()

# now repeat steps in 4.1 and answer the question in 4.4
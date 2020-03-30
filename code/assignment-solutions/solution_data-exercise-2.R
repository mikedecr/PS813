# ----------------------------------------------------
#   Data Exercise 2
#   Code to get started
# ----------------------------------------------------

# load packages
library("tidyverse")
library("broom")

# run this code to install program
source("https://raw.githubusercontent.com/mikedecr/PS813/master/code/programs/create-exercise-2.R")

# run program to create data
# replace NA with your seed number
d2 <- 
  create_exercise_2(seed = 33997) %>%
  print()



# ---- estimate regression -----------------------

# suppose we had a model of every independent variable
simple_model <- 
  lm(cost_per_household ~ hholds + density + wage + snowdays, data = d2) %>%
  print()

tidy(simple_model)
glance(simple_model)

# plot residuals. Notice a bit of a nonrandom shape
simple_model %>%
  augment() %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0)

# plot residuals against all predictors
augment(simple_model) %>%
  pivot_longer(
    cols = c(hholds, density, wage, snowdays),
    names_to = "predictor",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value, y = .resid)) + 
  facet_wrap(~ predictor, scales = "free") +
  geom_hline(yintercept = 0) +
  geom_point()


# the `hholds` predictor is showing a similar parabolic shape.
# this is consistent with the hint in the assignment sheet:
# "there may be some number of households at which [y] is minimized"

# so we try a model with a parabolic shape for the `hholds` variable
# y ~ hholds + hholds^2 + ...


# ---- estimating model w/ squared term -----------------------

# We can create a new variable that squares hholds.
d2_sq <- d2 %>%
  mutate(hholds_sq = hholds^2) %>%
  print()

# OR you can square it during estimation.
# Supply functions of variables in lm() formula with I()
# This comes in handy later
sq_mod <- lm(
  cost_per_household ~ hholds + I(hholds^2) + density + wage + snowdays, 
  data = d2
)

tidy(sq_mod)
glance(sq_mod)

# much better
sq_mod %>%
  augment() %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point()



# ---- How does my city compare to the model's prediction? ------------

# make a data frame for my city's data
my_city <- 
  tibble(
    cost_per_household = 48.5,
    hholds = 6.28,
    density = 620,
    wage = 19.50,
    snowdays = 5
  ) %>%
  print()


# use augment() to generate a model prediction for my city
augment(sq_mod, newdata = my_city)

# I will make a data frame comparing the two model predictions
#   for squared hholds and not.
# I will add confidence intervals using the model's critical t-values.
# - Since the models have different degrees of freedom, 
#   I use different t-values for each model.

# NOTICE: For this application I'm going to use 99% confidence intervals.
# These intervals will be wider than 95% intervals.
# I'm doing this because I want a broader picture of my model's 
#   picture of the world.
simple_t <- qt(p = .995, df = glance(simple_model)$df.residual)
sq_t <- qt(p = .995, df = glance(sq_mod)$df.residual)

# bind_rows() combines the augment results.
# Then I add confidence bounds depending on the model
city_means <- 
  bind_rows(
    "Incorrect Model" = 
      augment(simple_model, newdata = my_city) %>%
      mutate(critical_t = simple_t),
    "Correct Model" = 
      augment(sq_mod, newdata = my_city) %>%
      mutate(critical_t = sq_t),
    .id = "model"
  ) %>%
  mutate(
    conf.low = .fitted - (critical_t * .se.fit),
    conf.high = .fitted + (critical_t * .se.fit)
  ) %>%
  print()


# these predictions are for the predicted LINE only
#   (the predicted mean of y given x).
# If we wanted to generate a confidence interval for new DATA POINTS,
#   the interval formula contains the residual standard error as well.
# I will add variables for each models residual std.err: sigma
#   i.e. the square root of the residual variance.
# We use .se.fit (the standard error of the prediction line)
#   and sigma (the standard error of the residuals) 
#   to calculate the standard error of a new draw.
# Then make the prediction bounds by multiplying that by the critical t
city_predictions <- city_means %>%
  mutate(
    sigma = case_when(
      model == "Incorrect Model" ~ glance(simple_model)$sigma,
      model == "Correct Model" ~ glance(sq_mod)$sigma
    ),
    .se.draw = sqrt(.se.fit^2 + sigma^2),
    pred.low = .fitted - (critical_t * .se.draw),
    pred.high = .fitted + (critical_t * .se.draw)
  ) %>%
  print()

# Look only at predictions and intervals.
# Naturally, the prediction interval for a new data point
#   is wider than the confidence interval for the mean
city_predictions %>%
  select(model, .fitted, starts_with("conf."), starts_with("pred."))

# Let's plot these two predictions next to each other.
# In my final memo, I would only want to report my "best" model.
# But I'm showing you both to show you how modeling decisions affect 
#   your inferences and recommendations.

# I want to plot our city's current and proposed costs to the graphic
current_cost <- 48.5
proposed_cost <- 40.6




# WARNING:
# This graphic will contain a BUNCH of stuff.
# I'm doing this to show you what you can do
#   to make a graphic more informative
#   for a less stats-inclined audience

# let's make some colors
conf_color <- viridisLite::viridis(n = 1, begin = .25, end = .25)
pred_color <- viridisLite::viridis(n = 1, begin = .5, end = .5)

# geom_pointrange plots a point and error bar
# geom_linerange plots only the error bar (for the pred. interval)
ggplot(city_predictions) +
  aes(x = fct_rev(model), y = .fitted) +
  coord_flip() +
  geom_linerange(
    aes(ymin = pred.low, ymax = pred.high),
    color = pred_color,
    size = 1.25
  ) +
  geom_linerange(
    aes(ymin = conf.low, ymax = conf.high),
    size = 1.75,
    color = conf_color
  ) +
  geom_point(color = conf_color, size = 5) +
  labs(x = NULL, y = "Estimated Cost Per Household") +
  geom_hline(yintercept = current_cost) +
  annotate(
    geom = "text", fontface = "bold",
    x = 1.5, y = current_cost,
    label = "Current Cost",
    angle = -90,
    size = 4,
    vjust= -1
  ) +
  geom_hline(yintercept = proposed_cost, linetype = "dashed") +
  annotate(
    geom = "text", fontface = "bold",
    x = 1.5, y = proposed_cost,
    label = "Contractor Proposal",
    angle = -90,
    size = 4,
    vjust= -1
  ) +
  annotate(
    geom = "text", fontface = "bold",
    label = "Average estimated\ncost for our city",
    x = 2,
    y = city_predictions %>%
        filter(model == "Correct Model") %>%
        pull(conf.low),
    color = conf_color,
    size = 4,
    vjust = -1, hjust = 0
  ) +
  annotate(
    geom = "text", fontface = "bold",
    label = "99% of estimates\nfor our city",
    x = 2,
    y = city_predictions %>%
        filter(model == "Correct Model") %>%
        pull(pred.low),
    color = pred_color,
    size = 4,
    vjust = 2, hjust = 0
  ) +
  annotate(
    geom = "text", fontface = "bold", 
    label = "Notice how modeling choices\ninfluence policy recommendation!",
    x = 1, 
    y = city_predictions %>%
        filter(model == "Incorrect Model") %>%
        pull(conf.low),
    size = 4, 
    vjust = 2
  ) +
  theme_minimal(base_size = 14) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1))



# ---- commentary -----------------------

# We're trying to figure out if it makes sense to hire the contractor,
#   or if we could improve management without the contractor.
# Our (best) model shows that our city is an outlier:
# Nearly all cities with similar features as ours have lower costs.
# However, notice that the contractor's proposal does not get us 
#   within the range of other cities either! 
# Their proposal is also outside of what we should expect
#   for a city like ours.
# Right now, it seems like it makes more sense to improve management without 
#   hiring the private contractor.

# CAN YOU SEE HOW THIS INTERPRETATION WOULD BE DIFFERENT
#   IF WE USED A MODEL WITH ONLY LINEAR TERMS




# ---- costs of improved management -----------------------

# We can be more precise about this decision by considering how much 
#   money we have to work with.

# For instance, suppose we hire the the contractor; 
#   how much does this save us?
# Here is our annual pickup cost
scales::dollar(my_city$hholds * 10000 * current_cost)

# Here is the savings from using the contractor.
# We would save some money, 
#   but we wouldn't be close to the model's predictions.
scales::dollar(
  (my_city$hholds * 10000 * current_cost) - 
  (my_city$hholds * 10000 * proposed_cost)
)

# Here's one way to think about investing in better management. 
# Suppose that we could hit certain cost-per-household benchmarks...
#   how much would we save?
# Knowing those savings, we can think about that as being a "budget" 
#   for how much we can invest in improved management 
#   while still improving on our current costs.
# As long as our investment costs come in under our savings,
#   we will save money. 

city_predictions %>% 
  filter(model == "Correct Model") %>%
  select(.fitted, starts_with("conf."), starts_with("pred.")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "benchmark",
    values_to = "cost_estimate"
  ) %>%
  mutate(
    estimated_total_cost = (my_city$hholds * 10000) * cost_estimate,
    current_total_cost = (my_city$hholds * 10000) * current_cost,
    savings = current_total_cost - estimated_total_cost
  ) %>%
  arrange(savings) %>%
  mutate_if(is.numeric, scales::dollar) %>%
  print()


# If the salary of our current Sanitation Dept. supervisor is W,
#   and the salary of a more competent supervisor is Z,
# Then as long as the estimated savings under the new supervisor 
#   are greater than (Z - W), we will be saving money as a city.






# ---- plot the predicted effect of hholds^2 -----------------------

# suppose that you wanted to plot the quadratic fit...here's how.
# let's show how E[Y | X] changes as hholds changes.

# we need to create a dataset where hholds varies, 
# and then predict Y for that new dataset

# here's how we can do this do this.
# (run line by line to see each command's effects)
# - start with original data 
# - summarize_all() to get the mean of every variable
# - drop hholds (we'll recreate it) and the DV
# - cross the data with a sequence of possible hholds values
# Result: everything fixed at mean except hholds
hh_data <- d2 %>%
  summarize_all(mean) %>%
  select(-hholds, -cost_per_household) %>%
  crossing(
    hholds = seq(from = min(d2$hholds), to = max(d2$hholds), by = .1)
  ) %>%
  print()

# now we have a dataset where hholds varies
# and everything else is held at the mean

# I'm going to do one more trick to add partial residuals from the model.
partial_residuals <- d2 %>%
  summarize_all(mean) %>%
  select(-hholds, -cost_per_household) %>%
  crossing(hholds = d2$hholds) %>%
  augment(sq_mod, newdata = .) %>%
  left_join(augment(sq_mod) %>% select(hholds, .resid)) %>%
  print()



# use the t-stat to create confidence intervals
# and sigma to create prediction intervals (if desired)
sq_sigma <- glance(sq_mod)$sigma

hh_predictions <- 
  augment(sq_mod, newdata = hh_data) %>%
  mutate(
    conf.low = .fitted - (sq_t * .se.fit),
    conf.high = .fitted + (sq_t * .se.fit),
    pred.low = .fitted - (sq_t * sqrt(.se.fit^2 + sq_sigma^2)),
    pred.high = .fitted + (sq_t * sqrt(.se.fit^2 + sq_sigma^2))
  ) %>%
  print()


# predicted values as f(hholds)
ggplot(hh_predictions) +
  aes(x = hholds, y = .fitted) +
  geom_ribbon(
    aes(ymin = pred.low, ymax = pred.high),
    alpha = 0.2, color = NA
  ) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.5, color = NA
  ) +
  geom_line() +
  geom_point(
    data = partial_residuals,
    aes(y = .fitted + .resid)
  )




# ---- BONUS: splines -----------------------

# Dave prefers quadratic relationships over log relationships
#   because they can "turn" in ways that logs can't.
# Personally I am sometimes afraid of quadratic relationships 
#   (and polynomial relationships more generally)
#   because they MUST turn. 
# A squared term MUST turn one time. A cubed term MUST turn two times.
# In some situations, this isn't a problem. 
# In other situations, I'm scared of the model predicting crazy things
#   where there isn't a lot of data.

# One thing you may consider is SPLINES.
# A spline is a way to fit a nonlinear relationship to some X.
# It decomposes f(X) into a weighted combination of BASIS FUNCTIONS.

# How do splines work?
# An example: https://twitter.com/mikedecr/status/1241233439745101825/photo/1
# Panel 1: We create "k" many basis functions, which are functions of X.
#          b_{k}(x)
# Panel 2: Basis functions enter the model as predictors, so they each 
#          get a coefficient: β_{k} * b_{k}(x)
# Panel 3: x's overall influence on y is the sum of the basis functions 
#          and their coefficients

# Why is this helpful?
# You can fit wiggly functions of X with fewer functional form restrictions.
# The wiggliness of the line can be sensitive to tuning parameters, 
#   but this is similar to including too many polynomial terms (overfitting).
# With splines, you can penalize the estimation methods to prevent the 
#   fitted line from being unreasonably wiggly.
# The penalization does bias the fitted line (bias toward flatness) 
#   but in exchange for better out-of-sample prediction.


# ---- how to fit splines -----------------------

# use this package
library("mgcv")

# default spline: "thin plante" spline
# but you can specify other basis functions in s() using bs = ...
# here is an example using "ps" or penalized spline
spline_model <- 
  gam(
    cost_per_household ~ s(hholds, bs = "ps") + density + wage + snowdays,
    data = d2
  ) %>%
  print()

# learn more about fitting and evaluating spline models here:
#   https://noamross.github.io/gams-in-r-course/
# This is a non-technical introduction.
# For technical details, consult Wood (2017): "Generalized Additive Models"


# broom functions work for spline models as well

spline_DFs <- glance(spline_model)$df.residual
spline_T <- qt(p = .975, df = spline_DFs)
spline_sigma_sq <- spline_model$sig2

# generate predictions and conf int
spline_predictions <- d2 %>%
  summarize_all(mean) %>%
  select(-hholds) %>%
  crossing(hholds = d2$hholds) %>%
  augment(spline_model, newdata = .) %>%
  mutate(
    conf.low = .fitted - (spline_T * .se.fit),
    conf.high = .fitted + (spline_T * .se.fit),
    pred.low = .fitted - (spline_T * sqrt(.se.fit^2 + spline_sigma_sq)),
    pred.high = .fitted + (spline_T * sqrt(.se.fit^2 + spline_sigma_sq)),
  ) %>%
  print()

# plot over hholds, add residuals
ggplot(spline_predictions) +
  aes(x = hholds, y = .fitted) +
  geom_ribbon(
    aes(ymin = pred.low, ymax = pred.high),
    alpha = 0.2, color = NA
  ) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    alpha = 0.5, color = NA
  ) +
  geom_line() +
  geom_point(aes(y = .fitted + augment(spline_model)$.resid))

# Notice that the curve doesn't bend upward as much...
# This is because the polynomial WANTS to turn, but the spline is agnostic.
# The polynomial consults the wiggliness on the right side of the plot 
#   to inform wiggliness on the left side,
#   whereas the spline focuses more on "local" data and penalizes wiggliness.


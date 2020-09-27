# ----------------------------------------------------
#   Data Exercise 4: 
#   Code to get started
#   Provided by Michael DeCrescenzo, Spring 2020
# ----------------------------------------------------

library("tidyverse")
library("broom")

d4 <- create_exercise_4(456) %>% print()


# estimating a logit with glm()
# ("generalized linear model")
mod <- glm(
  Probat ~ Take + Report + Night + Convict,
  family = binomial(link = "logit"),
  data = d4
)

tidy(mod)

# here is the gist of a logit model:
# y is binary {0, 1} with "success" probability p:
#   y ~ Bernoulli(p) 

# we don't predict p directly; we predict a nonlinear function of p
#   called the "log odds"
#   odds: p / (1 - p)
#   log odds: log(p / (1 - p))
#   (also called the "logit" of p)
#   model says: logit(p) = XB 

# ==> p isn't linearly related to X, but logit(p) is

# can rewrite as
#   p = invlogit(XB)
#   as in, "inverse logit"
#   or, p = 1 / (1 + exp(-XB))
# The inverse logit is sometimes called the "logistic" function.
# It is the CDF of the standard logistic distribution.

# All generalized linear models have this same basic format:
# Data are draws from some distribution: y ~ D(mu)
# Parameters of the distribution (for ex. the mean) 
#   are predicted w/ covariates.
#   g(mu) = XB
#   where g() could be some transformation of the parameter 
#   (called a "link function")
# in MLE, coefficients B are estimated by maximizing 
#   the log-likelihood LL(Y; mu) w.r.t the coefficients B

# for OLS, y ~ Normal(mu, sigma)
# and g(mu) = XB where g(x) = x
# a.k.a. the "identity link" (i.e. no transformation)

# for logit, y ~ bernoulli(mu)
# logit(mu) = XB



# ---- predictions from logit -----------------------

# the model makes LINEAR PREDICTIONS on the logit scale.
# In order to transform them into probabilities, we must transform back.

# a demonstration
# simple predictions:
logit_predictions <- augment(mod) %>%
  print()

# notice that the predictions are not in (0, 1)
ggplot(logit_predictions) +
  aes(x = .fitted) +
  geom_histogram()

# if we wanted predictions on the probability scale,
#   we predict on the log scale,
#   then apply inverse-logit (logistic CDF)
# use plogis(): cumulative probability of the logistic distribution

# demonstration: scatter log odds (x) and predicted probability (y)
# with 95% CIs
# (CIs don't look smooth in this example because .se.fit depends on X data)
augment(mod) %>%
  mutate(
    .prob = plogis(.fitted),
    conf.low = plogis(.fitted - (qnorm(.975) * .se.fit)),
    conf.high = plogis(.fitted + (qnorm(.975) * .se.fit)),
  ) %>%
  ggplot() +
  aes(x = .fitted, y = .prob) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))




simple_data <- d4 %>%
  summarize(
    Take = mean(Take),
    Convict = median(Convict),
    Night = round(mean(Night))
  ) %>%
  crossing(Report = c(0, 1)) %>%
  augment(mod, newdata = .) %>%
  mutate(
    conf.low = .fitted - qnorm(.95) * .se.fit,
    conf.high = .fitted + qnorm(.95) * .se.fit
  ) %>%
  mutate_at(
    .vars = vars(.fitted, conf.low, conf.high),
    .funs = plogis
  ) %>%
  print()


ggplot(simple_data) +
  aes(x = Convict, y = .fitted, color = as.factor(Report)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  )


conv_data <- d4 %>%
  summarize(
    # Convict = median(Convict),
    Take = mean(Take),
    Night = round(mean(Night))
  ) %>%
  crossing(
    Report = c(0, 1), 
    Convict = min(d4$Convict):max(d4$Convict)
  ) %>%
  augment(mod, newdata = .) %>%
  mutate(
    conf.low = .fitted - qnorm(.95) * .se.fit,
    conf.high = .fitted + qnorm(.95) * .se.fit
  ) %>%
  mutate_at(
    .vars = vars(.fitted, conf.low, conf.high),
    .funs = plogis
  ) %>%
  print()

ggplot(conv_data) +
  aes(x = Convict, y = .fitted, color = as.factor(Report)) +
  geom_pointrange(
    aes(ymin = conf.low, ymax = conf.high),
    position = position_dodge(width = -0.25)
  )
  
  
  
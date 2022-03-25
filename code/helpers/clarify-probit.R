library("here")
library("tidyverse")
library("broom")
library("mvtnorm")

source(here("code", "programs", "create-exercise-4.R"))

dd <-
    create_exercise_4(seed = 7368633) |>
    print()

mod <- glm(
    Probat ~ (Take * Report) + Night + Convict,
    data = dd,
    family = binomial(link = "probit")
)

summary(mod)

# demo the matrix of coefficient draws
(Mu <- coef(mod))
(Sigma <- vcov(mod))
(sim_beta <- rmvnorm(n = 10, mean = Mu, sigma = Sigma))

# demo predictions for arbitrary data
new_data <- as_tibble(model.matrix(mod))
# assume we set let Take vary freely and set Report = to 1
# and predict at median of all else.
# we need to remake the interaction column to reflect Report
new_data <- new_data |>
    mutate(
        Report = 1,
        `Take:Report` = Take * Report,
        across(.cols = -c(Take, Report, `Take:Report`), .fns = median)
    ) |>
    print()

predictions <- as.matrix(new_data) %*% t(sim_beta)

head(predictions)

pprobs <- pnorm(predictions)

# gets quantiles of every obs
apply(pprobs, MARGIN = 1, quantile, c(0.025, .975))
# reshape
bounds <- t(apply(pprobs, MARGIN = 1, quantile, c(0.025, .975)))
head(bounds)

new_data <- new_data |>
    mutate(
        yhat = pnorm(predict(mod, new_data)),
        lower = bounds[, 1],
        upper = bounds[, 2]
    )

ggplot(new_data) +
    aes(x = Take) +
    geom_line(aes(y = yhat)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .1)

# ----------------------------------------------------------------------------
#  make functions
# ----------------------------------------------------------------------------

draw_coefs <- function(model, draws) {
    return(rmvnorm(n = draws, mean = coef(model), sigma = vcov(model)))
}

draw_coefs(mod, draws = 10)

# this function eats coefs, doesn't generate them.
# this is because you may want to 
#   re-use the same coefs multiple times if calculating contrasts
predicted_draws <- function(model, coefs, newdata = NULL) {
    if ((missing(newdata) || is.null(newdata))) {
        warning("newdata not supplied, using model.matrix(model)")
        newdata <- model.matrix(mod)
    }
    # ensure predictor matrix. Names must match coefs
    X <- as.matrix(newdata)
    stopifnot(colnames(newdata) == colnames(coefs))
    # Yhat = XB
    return(X %*% t(coefs))
}

predicted_draws(mod, draw_coefs(mod, 10))

predicted_draws(mod, 10, new_data)

compute_bounds <- function(Y, confidence) {
    stopifnot(confidence > 0 && confidence < 1)
    tail_prob <- (1 - confidence) / 2
    qts <- c(0 + tail_prob, 1 - tail_prob)
    rv <- t(apply(Y, MARGIN = 1, FUN = quantile, qts))
    colnames(rv) <- c("lower", "upper")
    return(rv)
}

head(compute_bounds(predicted_draws(mod, draw_coefs()), .95))


model.matrix(mod)

# ---- routine w/ functions --------

new_data <- as_tibble(model.matrix(mod))

# assume we set let Take vary freely and alternate Report
# and predict at median of all else.
# we need to remake the interaction column to reflect Report
new_data <- new_data |>
    select(-Report) |>
    crossing(Report = c(0, 1)) |>
    mutate(
        `Take:Report` = Take * Report,
        across(.cols = -c(Take, Report, `Take:Report`), .fns = median)
    ) |>
    # fixed name order
    select(one_of(names(coef(mod)))) |>
    print()

coefs <- draw_coefs(mod, 100000)
preds <- predicted_draws(mod, coefs, new_data)
link_bounds <- pnorm(compute_bounds(preds, .95))

new_data <- new_data %>%
    mutate(
        yhat = pnorm(predict(mod, newdata = .)),
        lower = link_bounds[, 'lower'],
        upper = link_bounds[, 'upper'],
    )

ggplot(new_data) +
    aes(x = Take, fill = as.factor(Report), color = as.factor(Report)) +
    geom_line(aes(y = yhat)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .3, color = NA)


# ---- contrasts example -------- 

new_data <- as_tibble(model.matrix(mod))

# assume we set let Take vary freely and alternate Report
# and predict at median of all else.
# we need to remake the interaction column to reflect Report
R1 <- new_data |>
    mutate(
        Report = 1,
        `Take:Report` = Take * Report,
        across(.cols = -c(Take, Report, `Take:Report`), .fns = median)
    ) |>
    print()

R0 <- new_data |>
    mutate(
        Report = 0,
        `Take:Report` = Take * Report,
        across(.cols = -c(Take, Report, `Take:Report`), .fns = median)
    ) |>
    print() 

coefs <- draw_coefs(mod, 100000)
P1 <- predicted_draws(mod, coefs, R1)
P0 <- predicted_draws(mod, coefs, R0)
# probability conversion before differengin
P <- plogis(P1) - plogis(P0)
link_diff_bounds <- compute_bounds(P, .95)

contrast_data <-
    new_data |>
    select(-Report) |>
    mutate(
        yhat_1 = pnorm(predict(mod, R1)),
        yhat_0 = pnorm(predict(mod, R0)),
        yhat_diff = yhat_1 - yhat_0,
        diff_lower = link_diff_bounds[, 'lower'],
        diff_upper = link_diff_bounds[, 'upper']
    )

ggplot(contrast_data) +
    aes(x = Take) +
    geom_line(aes(y = yhat_diff)) +
    geom_ribbon(aes(ymin = diff_lower, ymax = diff_upper), alpha = .3, color = NA)

# predicted_draws <- function(mod, n, newdata) {
#     coefs <- coef_draws(mod)
#     predictions <- predict_for(coefs, newdata)
# }


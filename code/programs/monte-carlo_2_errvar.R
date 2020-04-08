# ----------------------------------------------------
#   Monte Carlo 2: Errors in variables
#   Translation into R of Dave Weimer's Stata program
#   by Mike DeCrescenzo, Spring 2020
# ----------------------------------------------------

# Procedure: 
#   1. Execute file to create function errvar()
#   2. Use errvar() to generate monte carlo data.

errvar <- function(
  sample_size = numeric(),
  intercept = numeric(),
  slope = numeric(),
  sd_v = numeric(),
  sd_disturbance = numeric(),
  cor_wx = numeric(),
  n_simulations = 1000,
  seed = NA
) {

  if (is.na(seed)) {
    stop("Supply a seed number")
  }

  message(
    str_glue("Estimating coefficients for {n_simulations} simulated datasets of sample size {sample_size}")
  )

  d <- 
    tibble(x = runif(n = sample_size)) %>%
    crossing(iteration = 1:n_simulations) %>%
    mutate(
      z = x + rnorm(n = n(), sd = sd_v),
      y = intercept + (slope * x) + rnorm(n = n(), sd = sd_disturbance),
      w = (x * cor_wx * (1 / sqrt(1 - cor_wx^2))) + rnorm(n = n())
    ) %>%
    group_by(iteration) %>%
    nest() %>%
    mutate(
      attenuated = map(.x = data, .f = ~ lm(y ~ z, data = .x)),
      stage_1 = map(.x = data, .f = ~ lm(z ~ w, data = .x)),
      data = map2(
        .x = data, 
        .y = stage_1, 
        .f = ~ mutate(.x, zhat = augment(.y)$.fitted)
      ),
      stage_2 = map(.x = data, .f = ~ lm(y ~ zhat, data =.x)),
      coefs = map2(
        .x = attenuated,
        .y = stage_2,
        .f = ~ tibble(b_z = coef(.x)["z"], b_zhat = coef(.y)["zhat"])
      )
    ) %>%
    select(iteration, coefs) %>%
    unnest(coefs) %>%
    ungroup()

}



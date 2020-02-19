# ----------------------------------------------------
#   Monte Carlo 1: Correlation with IV and Error
#   Translation into R of Dave Weimer's Stata program
#   by Mike DeCrescenzo
# ----------------------------------------------------

# Procedure: 
#   1. Execute file to create function corerr()
#   2. Use corerr() to generate monte carlo data.

corerr <- function(
  sample_size = numeric(),
  intercept = numeric(),
  slope = numeric(),
  sd_predictor = numeric(),
  sd_disturbance = numeric(),
  cor_disturbance = numeric(),
  simulations = 1000
) {

  message(
    str_glue("Estimating coefficients for {simulations} simulated datasets of sample size {sample_size}")
  )

  d <- 
    tibble(m = seq(from = 1, to = simulations, by = 1)) %>%
    group_by(m) %>%
    mutate(
      data = list(
        tibble(
          x = rnorm(n = sample_size, mean = 0, sd = sd_predictor), 
          disturbance = rnorm(n = sample_size, mean = 0, sd = sd_disturbance), 
          y_uncor = intercept + (slope * x) + disturbance, 
          y = y_uncor + 
              ((cor_disturbance * sd_disturbance * x) / 
               (sqrt(1 - cor_disturbance^2) * sd_predictor))
          )
      ),
      model = map(data, ~ lm(y ~ x, data = .x)),
      intercept = map(model, ~ coef(.x)["(Intercept)"]),
      slope = map(model, ~ coef(.x)["x"])
    ) %>%
    select(iteration = m, intercept, slope) %>%
    unnest(cols = c(intercept, slope))

  return(d)

}


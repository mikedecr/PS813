# ----------------------------------------------------
#   Create PS 813 (Weimer) Data Exercise 2 in R
#   by Michael DeCrescenzo (mgdecrescenzo@gmail.com)
#   Modeled after <https://faculty.polisci.wisc.edu/weimer/PS813_EX2.ado>
# ----------------------------------------------------

create_exercise_2 <- function(seed = NA) {

  # set seed
  if (is.na(seed)) stop("Supply a seed number")
    
  set.seed(seed)

  # create data: 
  # 1. dave's hand-picked values
  # 2. create Y with error
  tibble(
    hholds = c(
      3.35, 11.19, 9.482, 9.43, 11.3, 6.18, 2.82, 2.95, 6.98, 7.89, 
      6.24, 8.97, 9.89, 6.74, 7.19, 11.11, 8.02, 6.86, 12.23, 8.87, 
      2.56, 4.19, 6.95, 10.91, 8.49, 3.56, 5.19, 7.95, 11.91, 7.49
    ),
    density = c(
      565.2, 739.8, 539.8, 629.4, 684.7, 605.3, 510.5, 458.8, 507.1, 524.1,
      593.6, 598.9, 685.2, 670.3, 684.2, 702.2, 743.8, 569.3, 654.9, 642.3, 
      632.9, 495.6, 673.0, 464.6, 699.2, 638.9, 595.6, 573.0, 468.6, 679.2
    ),
    wage = c(
      18.22, 15.71, 17.42, 19.64, 20.61, 20.21, 16.51, 15.77, 16.52, 19.87, 
      15.86, 15.66, 18.21, 20.50, 19.01, 16.08, 20.21, 21.36, 15.61, 17.14, 
      16.28, 17.01, 16.70, 20.52, 15.61, 16.58, 17.21, 16.90, 20.72, 18.61
    ),
    snowdays = c(
      3, 10, 3, 1, 6, 6, 4, 2, 3, 2, 
      4, 9, 6, 6, 7, 9, 6, 4, 5, 0, 
      2, 4, 1, 10, 3, 1, 3, 2, 9, 3
    )
  ) %>%
  mutate(
    cost_per_household = 
      (12.5 + 
       (0.8 * hholds^2) + 
       (-8 * hholds) + 
       (-0.01 * density) + 
       (2.0 * wage) + 
       (0.4 * snowdays) + 
       rnorm(n = n(), sd = sqrt(16))) %>%
      round(digits = 2)
  )
}

# cost_per_household = 21.5 + 0.8*hholds*hholds - 8.0*hholds - 0.01*density + 
#   2.0*wage + 0.4*snowdays + sqrt(16)*invnorm(uniform()) ; 
# quietly replace cost_per_household = round(cost_per_household,.01)
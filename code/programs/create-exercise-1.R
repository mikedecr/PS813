# ----------------------------------------------------
#   Create PS 813 (Weimer) Data Exercise 1 in R
#   by Michael DeCrescenzo (mgdecrescenzo@gmail.com)
#   Modeled after <https://faculty.polisci.wisc.edu/weimer/PS813_EX1.ado>
# ----------------------------------------------------

create_data_exercise_1 <- function(seed = numeric(0)) {
  
  # expect an integer seed of length 1
  if ((length(seed) == 1) == FALSE) {
    stop("seed must be length 1")
  }
  if (is.numeric(seed) == FALSE) {
    stop("seed must be numeric")
  }
  if ((seed %% round(seed) == 0) == FALSE) {
    stop("seed must be a whole number")
  }
   

  # make disturbance term for n observations
  set.seed(seed)
  n <- 20
  dis <- round(sqrt(50) * rnorm(n = n))
  
  # legislator data: term length and disturbance
  # in the future might make `terms` robust to arbitrary n
  data <- data.frame(
    legislator_id = seq(1, n, 1),
    terms = c(
      rep(1, 2),
      rep(2, 4),
      rep(3, 4),
      rep(4, 4),
      rep(5, 3),
      rep(6, 2),
      rep(7, 1)
    ),
    Leg_Act = 10 + dis
  )

  confirmation <- paste("\n<< Data generated from seed", seed, " >>\n")
  message(confirmation)
  return(data)
}


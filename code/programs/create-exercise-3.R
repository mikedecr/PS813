# ----------------------------------------------------
#   Data Exercise 3
#   R modification of Dave's 
#   <https://faculty.polisci.wisc.edu/weimer/PS813_EX3.ado>
# ----------------------------------------------------

# requires tidyverse

create_exercise_3 <- function(seed = NA) {

  # set seed
  if (is.na(seed)) stop("Supply a seed number")
    
  set.seed(seed)

  # create data: 
  # 1. fixed number of men and women
  # 2. create Y with error
  n <- 60

  sim_data <- 
    data.frame(
      Rating = round(runif(n = 60, min = 40, max = 100)),
      Credits = round(runif(n = 60, min = 0, max = 20)),
      Sex = c(rep(0, times = 22), rep(1, times = 38))
    )

  sim_data[["Salary"]] <- 
    with(sim_data,
      48.5 + (0.2 * Rating)  + (0.15 * Rating * Sex) + 
      (- 4  * Sex) + 
      rnorm(n = n, mean = 0, sd = sqrt(1.6))
    )
  
  return(sim_data)

}


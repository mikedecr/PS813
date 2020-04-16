# ----------------------------------------------------
#   Data Exercise 4
#   R modification of Dave's 
#   <https://faculty.polisci.wisc.edu/weimer/PS813_EX4.ado>
# ----------------------------------------------------


# requires tidyverse?

create_exercise_4 <- function(seed = NA) {

  # force seed
  if (is.na(seed)) {
    stop("Provide a seed number")
  } else {
    set.seed(seed)
  }

  n_cases <- 280

  tibble(case = 1:n_cases) %>%
  mutate(
    Take = runif(n = n(), min = 400, max = 10000) %>% round(),
    Report = rbinom(n = n(), size = 1, p = 0.55),
    Night = rbinom(n = n(), size = 1, p = 0.45),
    Convict = sample(
      x = c(0:5),
      size = n(),
      replace = TRUE,
      prob = c(0.3, 0.2, 0.2, 0.1, 0.1, 0.1)
    ),
    z = 4 + (-2 * Convict) + (-.0001 * Take) + (-.1 * Night) + (1.2 * Report),
    p = plogis(z),
    Probat = rbinom(n = n(), size = 1, prob = p)
  ) %>%
  select(-z, -p)

}


# * Program to give draw of data for Exercise 4 *
# capture program drop PS813_EX4
# program define PS813_EX4
# quietly set seed 97731
# quietly drop _all
# quietly set obs 280 
# #delimit ;
# quietly generate Take = 400+10000*uniform() ; 
# quietly replace Take = round(Take) ;
# quietly generate Report = 0 ;
# quietly replace Report = 1 if uniform()>=.55 ;
# quietly generate Night = 0 ;
# quietly replace Night = 1 if uniform()>= .45 ;
# quietly generate cut1 = uniform() ;
# quietly generate Convict = 0 ; # 30
# quietly replace Convict = 1 if cut1 >= .3 ; # 20
# quietly replace Convict = 2 if cut1 >= .5 ; # 20
# quietly replace Convict = 3 if cut1 >= .7 ; # 10
# quietly replace Convict = 4 if cut1 >= .8 ; # 10
# quietly replace Convict = 5 if cut1 >= .9 ; # 10
# quietly set seed `1' ;
# quietly generate z = -2*Convict-.0001*Take-.1*Night+1.2*Report+4 ;
# quietly generate p = 1/(1+exp(-z)) ;
# quietly generate Probat = 0 ;
# quietly replace Probat = 1 if uniform()<=p ;
# drop z p cut1 ;
# end ;
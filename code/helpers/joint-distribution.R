# ----------------------------------------------------
#   Visualizing a multivariate normal distribution
# ----------------------------------------------------

library("tidyverse")

# install these two packages:
# install.packages("mvtnorm")
# install.packages("ggpointdensity")


# This picture shows a multivariate normal distribution:
# https://blogs.sas.com/content/iml/files/2012/07/mvnormalpdf.png

# a multivariate normal distribution is a JOINT distribution 
#   over more than one normally distributed variable.
# Each variable is MARGINALLY normal, so it has a mean and varaicne.
# But the variables may be correlated, such that p(x) ≠ p(x | z).


# ---- sampling the multivariate normal -----------------------

# I'm creating means and variance covariance matrix
# using the "Simul_Con.pdf" data

# I'm naming the elements in the vector just for clarity
# It doesn't change how the vector behaves.
# Each element is the mean for that dimension.
mean_vector <- 
  c("x" = 10.176, "z" = 5.377) %>%
  print()

# Variance-Covariance matrix.
# Diagonals are variances for each coefficient.
# Off-diagonals are covariances between coefficients.
vcov_matrix <- matrix(
  data = c(56, -43, 
           -43, 41),
  byrow = TRUE,
  nrow = 2,
  dimnames = list(names(mean_vector), names(mean_vector))
) %>%
print()


# draw random samples from the multivariate normal
samples <- 
  rmvnorm(
    n = 10000,
    mean = mean_vector,
    sigma = vcov_matrix
  ) %>%
  as_tibble() %>%
  print()


# the MARGINAL DISTRIBUTIONS 
#   (looking at only one variable at a time) 
#   are normal
ggplot(samples) +
  aes(x = x) +
  geom_histogram()

ggplot(samples) +
  aes(x = z) +
  geom_histogram()


# the JOINT DISTRIBUTION shows the correlation
# - the marginal distribution of X still crosses zero, 
#   but only for certain values of Z (hence the correlation)
# - the marginal distribution of Z still crosses zero,
#   but only for certain values of X.
# This shows how the individual confidence intervals for each coef cross zero,
#   but the coefficients are still "jointly significant"
#   i.e. we reject the null model using the F test
#   (H_0 = all slopes simultaneously zero)
ggplot(samples) +
  aes(x = x, y = z) +
  ggpointdensity::geom_pointdensity() +
  theme(legend.position = "none")




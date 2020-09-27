# ----------------------------------------------------
#   Guide to Monte Carlo Exercise #2
#   This will ONLY get you started!!! 
#   Follow assignment instructions & modify code accordingly.
# ----------------------------------------------------

library("tidyverse")
library("broom")

mc <- errvar(
  sample_size = 1000,
  intercept = 0,
  slope = 1,
  sd_v = 1,
  sd_disturbance = 5,
  cor_wx = 0.95,
  n_simulations = 1000,
  seed = 8796987
) %>%
  print()

MCs <- 
  crossing(
    sd_v = c(.01, .1, .5, 1),
    cor_wx = c(.3, .5, .7, .9)
  ) %>%
  mutate(
    monte_carlo = map2(
      .x = sd_v,
      .y = cor_wx,
      .f = ~ {
        errvar(
          sample_size = 1000,
          intercept = 0, 
          slope = 1, 
          sd_v = .x, 
          sd_disturbance = 5, 
          cor_wx = .y, 
          n_simulations = 200, 
          seed = 8796987 
        )
      }
    ) 
  ) %>%
  unnest(monte_carlo) %>%
  print()

MCs %>%
  group_by(sd_v, cor_wx) %>% 
  summarize_at(
    .vars = vars(b_z, b_zhat), 
    .funs = list(mean = mean, sd = sd)
  )

MCs %>%
  pivot_longer(
    cols = c(starts_with("b_"))
  ) %>%
  ggplot() +
  aes(x = value) +
  facet_wrap(
    ~ str_glue("cor = {cor_wx}") + str_glue("sd = {sd_v}"),
    scales = "free",
    nrow = 4
  ) +
  geom_vline(xintercept = 1) +
  geom_histogram(
    bins = 100,
    aes(fill = name),
    position = "identity",
    alpha = 0.5
 )


MCs %>%
  pivot_longer(
    cols = c(starts_with("b_"))
  ) %>%
  group_by(sd_v, cor_wx, name) %>%
  summarize(
    mean = mean(value),
    sd = sd(value),
    q10 = quantile(value, .10),
    q90 = quantile(value, .90)
  ) %>%
  ggplot(aes(x = cor_wx, y = mean, color = as.factor(sd_v))) +
  geom_hline(yintercept = 1) +
  geom_pointrange(
    aes(ymin = q10, ymax = q90),
    position = position_dodge(width = 0.05)
  ) +
  # geom_pointrange(
  #   aes(ymin = mean - sd, ymax = mean + sd),
  #   position = position_dodge(width = 0.05),
  #   size = 1
  # ) +
  facet_wrap(~ name, scales = "free_y") +
  scale_color_viridis_d(option = "magma", end = 0.9)

mc %>%
  summarize_at(vars(bz, bzhat), list(mean = mean, sd = sd))

  gather(key = coef, value = value, -iter) %>%
  ggplot() +
  aes(x = value, fill = coef) +
  geom_histogram(position = "identity", bins = 100)


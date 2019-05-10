conditions <- expand.grid(n = seq(5, 150, 5),
                          mu = seq(-2, 2, 0.25))
head(conditions)

library(purrr)

sim1 <- map2(conditions$n, conditions$mu, ~rnorm(.x, .y, sd = 10))
str(sim1)

library(tibble)
library(dplyr)
library(tidyr)

sim2 <- conditions %>%
  as_tibble() %>% # Not required, but definitely helpful
  mutate(sim = map2(n, mu, ~rnorm(.x, .y, sd = 10))) %>% 
  unnest()

library(fivethirtyeight)
library(stringr)

pulitzer <- pulitzer %>%
  select(newspaper, starts_with("num")) %>%
  gather(year_range, n, -newspaper) %>%
  mutate(year_range = str_replace_all(year_range, "num_finals", ""),
         year_range = str_replace_all(year_range, "_", "-")) %>%
  filter(year_range != "1990-2014")
pulitzer

library(ggplot2)
library(glue)
p <- pulitzer %>%
  group_by(newspaper) %>%
  nest() %>%
  mutate(plot = map2(data, newspaper, ~
                       ggplot(.x, aes(year_range, n)) +
                       geom_col(aes(fill = n)) +
                       scale_fill_distiller(type = "seq", 
                                            limits = c(0, max(pulitzer$n)),
                                            palette = "BuPu",
                                            direction = 1) +
                       ylim(0, max(pulitzer$n)) +
                       coord_flip() +
                       labs(title = glue("Pulitzer Prize winners: {.y}"))))
p$plot[[10]]
  
full_conditions <- expand.grid(n = seq(5, 150, 5),
                               mu = seq(-2, 2, 0.25),
                               sd = seq(1, 3, .1))
head(full_conditions)

fsim <- pmap(list(number = full_conditions$n,
                  average = full_conditions$mu,
                  stdev = full_conditions$sd), 
             function(number, average, stdev) {
               rnorm(n = number, mean = average, sd = stdev)
             })
str(fsim)

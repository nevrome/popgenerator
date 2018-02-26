#### create population ####

timeframe <- 0:0

population_settings <- new(
  "population_settings",
  time =  timeframe,
  population_size_function = population_size,
  unit_amount_function = unit_amount,
  age_distribution_function = age_distribution,
  age_range = 1:100,
  sex_distribution_function = sex_distribution,
  sex_range = c("male", "female"),
  unit_distribution_function = unit_distribution
)

test <- population_settings %>%
  generate_population()

#### create relations ####

relations_settings <- new(
  "relations_settings",
  population = test,
  monogamy_probability = 0.9,
  start_fertility_age = 15,
  stop_fertility_age = 50,
  same_unit_as_child_probability = 0.9,
  same_unit_as_partner_probability = 0.9
)

test2 <- relations_settings %>%
  generate_relations()

#### analyse result ####

library(ggplot2)

population_real <- test %>% count_living_humans_over_time(timeframe)
population_expected <- tibble::tibble(
  time = timeframe,
  n = population_size(time)
)

units_real <- test %>% count_living_units_over_time(timeframe)
units_expected <- tibble::tibble(
  time = timeframe,
  n = unit_amount(time)
)

population_development_plot <- ggplot() +
  geom_line(
    data = population_expected,
    aes(x = time, y = n),
    color = "black"
  ) +
  geom_line(
    data = population_real,
    aes(x = time, y = n),
    color = "red"
  )

unit_development_plot <- ggplot() +
  geom_line(
    data = units_expected,
    aes(x = time, y = n),
    color = "black"
  ) +
  geom_line(
    data = units_real,
    aes(x = time, y = n),
    color = "red"
  )

library(cowplot)
cowplot::plot_grid(
  population_development_plot,
  unit_development_plot,
  align = "v",
  nrow = 2,
  labels = "AUTO"
)

library(ggplot2)
test %>%
  ggplot() +
  geom_segment(
    aes(y = id, yend = id, x = birth_time, xend = death_time, color = unit)
  ) +
  facet_wrap(~unit) +
  geom_vline(aes(xintercept = timeframe[1])) +
  geom_vline(aes(xintercept = timeframe[length(timeframe)]))



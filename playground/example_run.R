#### create artifical population ####

time <- 1:200
unit_names <- factor(c("group_1", "group_2", "group_3", "group_4"))
unit_size_functions <- list(
  "group_1" = function(t) {20 + 0.4 * t}, "group_2" = function(t) {20 + (0.04 * t)^2},
  "group_3" = function(t) {50}, "group_4" = function(t) {20 + 10 * log(t)}
)
age_distribution_function <-  function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}
age_range <- 1:90

settings <- popgenerator::init_population_settings(
  time, unit_names, unit_size_functions, age_distribution_function, age_range
)

population <- popgenerator::generate_population(settings)

#### plot population comparison graphs ####

pop_ts <- aoristAAR::aorist(population, "birth_time", "death_time", "unit")
theory_pop_ts <- do.call(rbind, lapply(
  unit_names, function(x) {
    tibble::tibble(
      date = time,
      sum = unit_size_functions[[x]](time),
      unit = unit_names[[x]]
    )
  }
))

library(ggplot2)
ggplot() +
  geom_line(
    data = theory_pop_ts,
    mapping = aes(x = date, y = sum),
    color = "red"
  ) +
  geom_line(
    data = pop_ts,
    mapping = aes(x = date, y = sum)
  ) +
  facet_wrap(~unit) +
  xlim(-40, 240) +
  geom_vline(
    xintercept = c(1, 200),
    color = "darkgreen"
  ) +
  theme_bw()

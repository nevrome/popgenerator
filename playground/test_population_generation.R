#### setup settings grid ####

all_model_populations <- expand.grid(
  #multiplier = 1:10,
  # general settings
  timeframe = list(
      0:0
    ),
  # population settings  
  population_size_functions = c(
      function(t) {round(2000 - 0.95 * t, 0)},
      function(t) {1000},
      function(t) {round(100 + 0.95 * t, 0)},
      function(t) {round(0.0019 * (t - 1000)^2 + 100, 0)},
      function(t) {round(-0.0019 * (t - 1000)^2 + 2000, 0)}
    ),
  unit_amount_functions = c(
      function(t) {round(20 - 0.0095 * t, 0)},
      function(t) {10},
      function(t) {round(1 + 0.0095 * t, 0)},
      function(t) {round(0.000019 * (t - 1000)^2 + 1, 0)},
      function(t) {round(-0.000019 * (t - 1000)^2 + 20, 0)}
    ),
  age_distribution_functions = c(
      function(t) {function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}}
    ),
  age_ranges = list(
      1:100
    ),
  sex_distribution_functions = c(
      function(t) {function(x) {rep(1/length(x), length(x))}}
    ),
  sex_ranges = list(
      c("male", "female")
    ),
  unit_distribution_functions = c(
      function(t) {function(x) {rep(1/length(x), length(x))}}
    ),
  # relations settings
  monogamy_probabilities = list(
      0,5,
      0.7,
      0.9
    ),
  start_fertility_ages = list(
      15
    ),
  stop_fertility_ages = list(
      50
    ),
  same_unit_as_child_probabilities = list(
      0,5,
      0.7,
      0.9
    ),
  same_unit_as_partner_probabilities = list(
      0,5,
      0.7,
      0.9
    ),
  child_of_weight_distribution_functions = c(
      function(t) {function(x) {x}}
    ),
  amounts_friends = list(
      0,
      10,
      50
    ),
  friendship_age_distribution_functions = c(
      function(t) {function(x) {0.5}},
      function(t) {function(x) {abs(1 - x * 0.02)}},
      function(t) {function(x) {stats::dt(x, df = 3)}}
    )
) %>% tibble::as.tibble()

plot_prep_grid(all_model_populations, "population_size_functions")
plot_prep_grid(all_model_populations, "unit_amount_functions")
plot_prep_grid(all_model_populations, "age_distribution_functions")
plot_prep_grid(all_model_populations, "friendship_age_distribution_functions")

all_model_populations %<>% init_population_settings()
all_model_populations[11615,] %>% generate_all_populations() -> test

test %<>% init_relations_settings()
test %>% generate_all_relations() -> test2


####

test2

g <- igraph::graph_from_data_frame(
  test2,
  directed = FALSE,
  vertices = test
)

RBioFabric::bioFabric(g)

karate_d3 <- networkD3::igraph_to_networkD3(g, group = test)

# Create force directed network plot
networkD3::forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'name')

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



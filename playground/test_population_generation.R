timeframe <- 0:2000

population_size_functions <- c(
  function(t) {round((cos(0.01 * t) + 3) * 100 + 0.2 * t, 0)},
  function(t) {1000}
)

unit_amount_functions <- c(
  function(t) {round((sin(0.02 * t) + 3) * 2, 0)},
  function(t) {10}
)

age_distribution_functions <- c(
  function(t) {function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}}
  #plot(0:100, age_distribution_functions[[1]](0)(0:100))
)

sex_distribution_functions <- c(
  function(t) {function(x) {rep(1/length(x), length(x))}}
)

unit_distribution_functions <- c(
  function(t) {function(x) {rep(1/length(x), length(x))}}
)

hu <- expand.grid(
  population_size_functions = population_size_functions,
  unit_amount_functions = unit_amount_functions,
  age_distribution_functions = age_distribution_functions,
  sex_distribution_functions = sex_distribution_functions,
  unit_distribution_functions = unit_distribution_functions
) %>% tibble::as.tibble()

hu %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    schnu = population_size_functions(timeframe)
  )

sapply(hu$population_size_functions, mapply, hu$time)

ggplot() +
  geom_line(
    aes(
      x = timeframe,
      y = population_size_functions[[1]](timeframe)
    )
  )

#### create population ####

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
  same_unit_as_partner_probability = 0.9,
  child_of_weight_distribution_function = weight_child_of_distribution,
  amount_friends = 10,
  friendship_age_distribution_function = friendship_age_distribution
)

test2 <- relations_settings %>%
  generate_relations()

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



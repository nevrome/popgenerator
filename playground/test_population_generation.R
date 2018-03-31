timeframe <- 0:10

#### population settings ####

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

age_ranges <- list(
  1:100,
  1:70
)

sex_distribution_functions <- c(
  function(t) {function(x) {rep(1/length(x), length(x))}}
)

sex_ranges <- list(
  c("male", "female")
)

unit_distribution_functions <- c(
  function(t) {function(x) {rep(1/length(x), length(x))}}
)

#### relations settings ####

monogamy_probabilities <- list(
  0.7,
  0.9
)

start_fertility_ages <- list(
  15
)

stop_fertility_ages <- list(
  50
)

same_unit_as_child_probabilities <- list(
  0.9
)

same_unit_as_partner_probabilities <- list(
  0.9
)

child_of_weight_distribution_functions <- c(
  function(t) {function(x) {x}}
)

amounts_friends = list (
  10
)

friendship_age_distribution_functions <- c(
  function(t) {function(x) {stats::dt(x, df = 3)}}
)

#### setup settings grid ####

all_model_populations <- expand.grid(
  #multiplier = 1:10,
  # population settings  
  population_size_functions =              population_size_functions,
  unit_amount_functions =                  unit_amount_functions,
  age_distribution_functions =             age_distribution_functions,
  age_ranges =                             age_ranges,
  sex_distribution_functions =             sex_distribution_functions,
  sex_ranges =                             sex_ranges,
  unit_distribution_functions =            unit_distribution_functions,
  # relations settings
  monogamy_probabilities =                 monogamy_probabilities,
  start_fertility_ages =                   start_fertility_ages,
  stop_fertility_ages =                    stop_fertility_ages,
  same_unit_as_child_probabilities =       same_unit_as_child_probabilities,
  same_unit_as_partner_probabilities =     same_unit_as_partner_probabilities,
  child_of_weight_distribution_functions = child_of_weight_distribution_functions,
  amounts_friends =                        amounts_friends,
  friendship_age_distribution_functions =  friendship_age_distribution_functions
) %>% tibble::as.tibble()

population_settings <- list() 
for (i in 1:nrow(all_model_populations)) {
  population_settings[[i]] <- new(
    "population_settings",
    time = timeframe,
    population_size_function =   all_model_populations$population_size_functions[[i]],
    unit_amount_function =       all_model_populations$unit_amount_functions[[i]],
    age_distribution_function =  all_model_populations$age_distribution_functions[[i]],
    age_range =                  all_model_populations$age_ranges[[i]],
    sex_distribution_function =  all_model_populations$sex_distribution_functions[[i]],
    sex_range =                  all_model_populations$sex_ranges[[i]],
    unit_distribution_function = all_model_populations$unit_distribution_functions[[i]]
  )
}

all_model_populations %<>%
  dplyr::mutate(
    population_settings = population_settings
  )

#### create population ####

all_model_populations %<>%
  dplyr::mutate(
    populations = lapply(all_model_populations$population_settings, generate_population)
  )

#### create relations ####

relations_settings <- list() 
for (i in 1:nrow(all_model_populations)) {
  relations_settings[[i]] <- new(
    "relations_settings",
    population =                            all_model_populations$populations[[i]],
    monogamy_probability =                  all_model_populations$monogamy_probabilities[[i]],
    start_fertility_age =                   all_model_populations$start_fertility_ages[[i]],
    stop_fertility_age =                    all_model_populations$stop_fertility_ages[[i]],
    same_unit_as_child_probability =        all_model_populations$same_unit_as_child_probabilities[[i]],
    same_unit_as_partner_probability =      all_model_populations$same_unit_as_partner_probabilities[[i]],
    child_of_weight_distribution_function = all_model_populations$child_of_weight_distribution_functions[[i]],
    amount_friends =                        all_model_populations$amounts_friends[[i]],
    friendship_age_distribution_function =  all_model_populations$friendship_age_distribution_functions[[i]]
  )
}

all_model_populations %<>%
  dplyr::mutate(
    relations_settings = relations_settings
  )

#### create population ####

all_model_populations %<>%
  dplyr::mutate(
    populations = lapply(all_model_populations$relations_settings, generate_relations)
  )

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



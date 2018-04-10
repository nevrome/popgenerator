#### setup settings grid ####

# create populations_grid data.frame
all_model_populations <- expand.grid(
  #multiplier = 1:10,
  # general settings
  timeframe = list(
    0:2000
  ),
  # population settings  
  population_size_functions = c(
    function(t) {1000},
    function(t) {round(2000 - 0.95 * t, 0)},
    function(t) {round(100 + 0.95 * t, 0)},
    function(t) {round(0.0019 * (t - 1000)^2 + 100, 0)},
    function(t) {round(-0.0019 * (t - 1000)^2 + 2000, 0)}
  ),
  units_amount = c(
    50,
    100,
    200
  ),
  age_distribution_functions = c(
    function(t) {function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}}
  ),
  age_ranges = list(
    1:100
  ),
  unit_distribution_functions = c(
    function(t) {function(x) {rep(1/length(x), length(x))}}
  ),
  # relations settings
  amounts_friends = list(
    10,
    10,
    50
  ),
  cross_unit_proportion_child_of = list(
    0.01,
    0.05,
    0.1,
    0.5
  ),
  cross_unit_proportion_friend = list(
    0.01,
    0.05,
    0.1,
    0.5
  ),
  child_of_weight_distribution_functions = c(
    function(t) {function(x) {x}}
  )
) %>% tibble::as.tibble()

plot_prep_grid(all_model_populations, "population_size_functions")
plot_prep_grid(all_model_populations, "age_distribution_functions")
plot_prep_grid(all_model_populations, "friendship_age_distribution_functions")

all_model_populations %<>% init_population_settings()
all_model_populations[1:10,] %>% generate_all_populations() -> test

test %<>% init_relations_settings()
test %>% generate_all_relations() -> test2

test$population_settings[[1]] -> settings 
test$relations_settings[[2]] -> settings

test2$populations[[1]] -> pop
test2$relations[[1]] -> rel

pop$id
hu <- rel[rel$type == "child_of", ]

hu

# g <- igraph::graph_from_data_frame(hu, directed = FALSE)
# g <- igraph::simplify(g)
# pu <- igraph::fastgreedy.community(g)
# 
# igraph::membership(pu)
# igraph::sizes(pu)
# 
# plot(pu)

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

test2$populations[[1]] -> pop
timeframe <- 0:2000

population_real <- pop %>% count_living_humans_over_time(timeframe)
population_expected <- tibble::tibble(
  time = timeframe,
  n = test$population_settings[[1]]@population_size_function(time)
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



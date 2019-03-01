library(magrittr)

#### prepare data ####

# groups
groups <- paste0("group_", 1:8)

# starting positions
start_proportion_5050 <- structure(
  list(
    idea_1 = c(.5, .5, .5, .5, .5, .5, .5, .5), 
    idea_2 = c(.5, .5, .5, .5, .5, .5, .5, .5)
  ), 
  class = "data.frame", 
  row.names = groups
)

# distance matrizes
distance_matrix_random <- matrix(runif(64, 0, 1), 8, 8) %>% `diag<-`(0)
distance_matrix_equal <- distance_matrix_random %>% `[<-`(1) %>% `diag<-`(0)

#### setup settings grid ####

populations_grid <- expand.grid(
  # general settings
  timeframe = list(
    0:200
  ),
  # population settings  
  unit_amount = c(
    8
  ),
  unit_names = list(
    groups
  ),
  unit_size_functions = list(
    list(
      "1" = function(t) {10},
      "2" = function(t) {10},
      "3" = function(t) {10},
      "4" = function(t) {10},
      "5" = function(t) {10},
      "6" = function(t) {10},
      "7" = function(t) {10},
      "8" = function(t) {10}
    )
  ),
  age_distribution_functions = c(
    function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}
  ),
  age_ranges = list(
    1:90
  ),
  # relations settings
  amounts_friends = c(
    10
  ),
  unit_interaction_matrix = list(
    distance_matrix_equal
  ),
  cross_unit_proportion_child_of = c(
    0.002
  ),
  cross_unit_proportion_friend = c(
    0.01
  ),
  weight_child_of = list(
    50
  ),
  weight_friend = list(
    10
  ),
  stringsAsFactors = FALSE
) %>% tibble::as_tibble()

save(populations_grid, file = "playground/test_populations_grid.RData")

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

models_grid <- expand.grid(
  # general settings
  timeframe = list(
    -2200:-800
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
    distance_matrix_equal,
    distance_matrix_random
  ),
  cross_unit_proportion_child_of = c(
    0.002, 0.02
  ),
  cross_unit_proportion_friend = c(
    0.01, 0.1
  ),
  weight_child_of = list(
    50
  ),
  weight_friend = list(
    10
  ),
  # ideas settings
  names = list(
    c("idea_1", "idea_2")
  ),
  start_distribution = list(
    start_proportion_5050
  ), 
  strength = list(
    c(1, 1) 
  ),
  stringsAsFactors = FALSE
) %>% tibble::as_tibble() %>%
  # remove unnecessary repetition
  dplyr::filter(
    5 * cross_unit_proportion_child_of == cross_unit_proportion_friend
  ) %>%
  # add relevant model ids
  dplyr::mutate(
    model_group = c(
      "low equal interaction",
      "low spatial interaction",
      "high equal interaction",
      "high spatial interaction"
    )
  ) %>%
  tidyr::uncount(100) %>%
  dplyr::mutate(
    model_id = 1:nrow(.)
  )

save(models_grid, file = "playground/test_models_grid.RData")

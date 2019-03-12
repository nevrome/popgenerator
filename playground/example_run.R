settings <- popgenerator::init_population_settings(  
  time = 0:200,
  unit_names = factor(c("group_1", "group_2", "group_3", "group_4")),
  unit_size_functions = list(
    "group_1" = function(t) {10}, "group_2" = function(t) {10},
    "group_3" = function(t) {10}, "group_4" = function(t) {10}
  ),
  age_distribution_function = function(x) {
    1 / (1 + 0.0004 * 0.7^(-7*log(x)))
  },
  age_range = 1:90
)

population <- popgenerator::generate_population(settings)


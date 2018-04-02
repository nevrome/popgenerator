#' generate population
#'
#' @param settings test
#'
#' @return huup
#'
#' @export
generate_population <- function(settings) {

  start_moment <- settings@time[1]
  intitial_population_size = settings@population_size_function(0)
  intitial_unit_size = settings@unit_amount_function(0)

  initial_population <- generate_humans(
    t = start_moment,
    n = intitial_population_size,
    start_id = 1,
    start_age = NA,
    settings = settings,
    unit_vector = 1:intitial_unit_size
  ) %>%
    dplyr::arrange(
      .data$birth_time
    ) %>%
    dplyr::mutate(
      id = 1:intitial_population_size
    )

  final_population <- initial_population %>% simulate_growth(
    time = settings@time,
    settings = settings
  )

  return(final_population)

}

#' generate_all_populations
#'
#' @param x all_model_populations
#'
#' @return huup
#'
#' @export
generate_all_populations <- function(x) {
  x %>% 
    dplyr::mutate(
      populations = lapply(x$population_settings, generate_population)
    )
}

#' init_population_settings
#'
#' @param x all_model_populations
#'
#' @return huup
#'
#' @export
init_population_settings <- function(x) { 

  population_settings <- list() 
  for (i in 1:nrow(x)) {
    population_settings[[i]] <- new(
      "population_settings",
      time =                       x$timeframe[[i]],
      population_size_function =   x$population_size_functions[[i]],
      unit_amount_function =       x$unit_amount_functions[[i]],
      age_distribution_function =  x$age_distribution_functions[[i]],
      age_range =                  x$age_ranges[[i]],
      sex_distribution_function =  x$sex_distribution_functions[[i]],
      sex_range =                  x$sex_ranges[[i]],
      unit_distribution_function = x$unit_distribution_functions[[i]]
    )
  }
  
  x %>%
    dplyr::mutate(
      population_settings = population_settings
    )

}

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
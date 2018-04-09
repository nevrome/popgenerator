#' population_settings class
#'
#' class to store values that should be passed to the population generator
#'
#' @slot time test
#' @slot population_size_function test
#' @slot units_amount test
#' @slot age_distribution_function test
#' @slot age_range test
#' @slot unit_distribution_function test
#' @slot start_unit_vector test
#'
#' @export
setClass(
  Class = "population_settings",
  slots = c(
    time = "numeric",
    population_size_function = "function",
    units_amount = "numeric",
    age_distribution_function = "function",
    age_range = "numeric",
    unit_distribution_function = "function",
    start_unit_vector = "numeric"
  )
)

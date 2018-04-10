#' population_settings class
#'
#' class to store values that should be passed to the population generator
#'
#' @slot time test
#' @slot population_size_function test
#' @slot units_amount test
#' @slot age_distribution_function test
#' @slot age_range test
#'
#' @export
setClass(
  Class = "population_settings",
  slots = c(
    time = "numeric",
    population_size_function = "function",
    units_amount = "numeric",
    age_distribution_function = "function",
    age_range = "numeric"
  )
)

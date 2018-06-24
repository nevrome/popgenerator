#' population_settings class
#'
#' class to store values that should be passed to the population generator
#'
#' @slot time test
#' @slot unit_name test
#' @slot unit_size_function test
#' @slot age_distribution_function test
#' @slot age_range test
#'
#' @export
setClass(
  Class = "unit_settings",
  slots = c(
    time = "numeric",
    unit_name = "character",
    unit_size_function = "function",
    age_distribution_function = "function",
    age_range = "numeric"
  )
)

#' init_unit_settings
#'
#'
#' @param x populations_settings object
#'
#' @return populations_grid data.frame with additional column 
#' population_settings
#'
#' @export
init_unit_settings <- function(x) { 
  
  # create list of units_settings objects from the populations_settings object
  unit_settings <- list() 
  for (i in 1:x@unit_amount) {
    unit_settings[[i]] <- methods::new(
      "unit_settings",
      time =                      x@time,
      unit_name =                 x@unit_names[[i]],
      unit_size_function =        x@unit_size_functions[[i]],
      age_distribution_function = x@age_distribution_function,
      age_range =                 x@age_range
    )
  }
  
  return(unit_settings)
}

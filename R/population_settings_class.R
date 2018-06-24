#' population_settings class
#'
#' class to store values that should be passed to the population generator
#'
#' @slot time test
#' @slot unit_amount test
#' @slot unit_names test
#' @slot unit_size_functions test
#' @slot age_distribution_function test
#' @slot age_range test
#'
#' @export
setClass(
  Class = "population_settings",
  slots = c(
    time = "numeric",
    unit_amount = "numeric",
    unit_names = "character",
    unit_size_functions = "list",
    age_distribution_function = "function",
    age_range = "numeric"
  )
)

#' init_population_settings
#'
#' Create population_settings object for every row in the 
#' populations_grid data.frame and add it in an additional
#' column.
#'
#' @param x populations_grid data.frame
#'
#' @return populations_grid data.frame with additional column 
#' population_settings
#'
#' @export
init_population_settings <- function(x) { 
  
  # create list of population_settings objects for each row of 
  # the input populations_grid data.frame
  population_settings <- list() 
  for (i in 1:nrow(x)) {
    population_settings[[i]] <- methods::new(
      "population_settings",
      time =                       x$timeframe[[i]],
      unit_amount =                x$unit_amount[[i]],
      unit_names =                 x$unit_names[[i]],
      unit_size_functions =        x$unit_size_functions[[i]],
      age_distribution_function =  x$age_distribution_functions[[i]],
      age_range =                  x$age_ranges[[i]]
    )
  }
  
  # add new list column with population_settings objects to the input grid
  x$population_settings <- population_settings
  
  return(x)
}

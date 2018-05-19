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
      population_size_function =   x$population_size_functions[[i]],
      units_amount =               x$units_amount[[i]],
      age_distribution_function =  x$age_distribution_functions[[i]],
      age_range =                  x$age_ranges[[i]]
    )
  }
  
  # add new list column with population_settings objects to the input grid
  x$population_settings <- population_settings
  
  return(x)
}

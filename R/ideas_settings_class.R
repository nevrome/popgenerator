#' ideas_settings class
#'
#' class to store idea definition values
#'
#' @slot population test
#' @slot names test
#' @slot start_distribution test
#' @slot strength test
#'
#' @export
setClass(
  Class = "ideas_settings",
  slots = c(
    population = "data.frame",
    names = "character",
    start_distribution = "numeric",
    strength = "numeric"
  )
)

#' init_ideas_settings
#'
#' Create ideas_settings object for every row in the 
#' populations_grid data.frame and add it in an additional
#' column.
#'
#' @param x populations_grid data.frame
#'
#' @return populations_grid data.frame with additional column 
#' ideas_settings
#'
#' @export
init_ideas_settings <- function(x) { 
  
  ideas_settings <- list() 
  for (i in 1:nrow(x)) {
    ideas_settings[[i]] <- methods::new(
      "ideas_settings",
      population =         x$populations[[i]],
      names =              x$names[[i]],
      start_distribution = x$start_distribution[[i]],
      strength =           x$strength[[i]]
    )
  }
  
  x$ideas_settings <- ideas_settings
  
  return(x)
}

#' idea_distribution_to_starting_nodes
#'
#' @param settings ideas_settings object
#'
#' @return a list of numeric vectors
#' 
#' @export
idea_distribution_to_starting_nodes <- function(settings) {
  
  number_of_ideas <- length(settings@names)
  
  population_by_unit <- split(settings@population, settings@population$unit)
  
  initial_ideas_by_unit <- unname(unlist(lapply(
    population_by_unit,
    function(population) {
      utils::head(population$id, 5)
    }
  )))
  
  starting_humans_per_idea <- list()
  index_border_start <- 1
  index_border_end <- 0
  for (i in 1:number_of_ideas) {
    index_border_end <- index_border_end + round(length(initial_ideas_by_unit) * settings@start_distribution[i])
    starting_humans_per_idea[[i]] <- initial_ideas_by_unit[
      index_border_start:index_border_end
    ]
    index_border_start <- index_border_end + 1
  }

  return(starting_humans_per_idea)  
}

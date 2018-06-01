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
    start_distribution = "data.frame",
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
  
  humans_at_time_zero_by_unit <- lapply(
    population_by_unit,
    function(population) {
      population$id[population$birth_time < 0 & population$death_time > 0]
    }
  )
  
  number_of_units <- length(population_by_unit)
  idea_1_pot <- c()
  idea_2_pot <- c()
  for (i in 1:number_of_units) {
    if (length(humans_at_time_zero_by_unit[[i]]) == 0) {
      next;
    }
    pot_split <- split(
      humans_at_time_zero_by_unit[[i]], 
      sample(
        2, 
        length(humans_at_time_zero_by_unit[[i]]), 
        prob = settings@start_distribution[1, ], 
        replace = TRUE
      )
    )
    idea_1_pot <- c(idea_1_pot, pot_split[["1"]])
    idea_2_pot <- c(idea_2_pot, pot_split[["2"]])
  }
  
  starting_humans_per_idea <- list(idea_1_pot, idea_2_pot)
  
  return(starting_humans_per_idea)  
}

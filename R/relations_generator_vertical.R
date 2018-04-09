#' generate_vertical_relations
#' 
#' Generate relations between parents and children.
#'
#' @param settings relations_settings object
#'
#' @return relations data.frame (every row contains one relation)
#'
#' @export
generate_vertical_relations <- function(settings) {

  population <- settings@population[order(settings@population$birth_time), ]
  
  population_by_unit <- split(population, population$unit)
  
  relations_by_unit <- lapply(
    population_by_unit,
    function(population) {
      humans <- population$id
      
      from <- rep(humans[-(1:100)], each = 2)
      to <- floor(
        stats::runif(
          length(from), 
          min = from - 100, 
          max = from
        )
      )
      
      vertical_relations <- tibble::tibble(
        from = from, 
        to = to, 
        type = "child_of"
      )
    }
  )

  all_relations <- do.call(rbind, relations_by_unit)
  
  return(all_relations)
  
}

#### helper functions ####

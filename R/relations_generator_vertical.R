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

  population_by_unit <- split(settings@population, settings@population$unit)
  
  relations_by_unit <- lapply(
    population_by_unit,
    function(population) {
      humans <- population$id
      
      from_index <- rep(101:length(humans), each = 2)
      to_index <- floor(
        stats::runif(
          length(from), 
          min = from_index - 100, 
          max = from_index
        )
      )
      
      from <- humans[from_index]
      to <- humans[to_index]
      
      vertical_relations <- tibble::tibble(
        from = from, 
        to = to, 
        unit = population$unit[1],
        type = "child_of"
      )
    }
  )

  all_relations <- do.call(rbind, relations_by_unit)
  
  return(all_relations)
  
}

#### helper functions ####

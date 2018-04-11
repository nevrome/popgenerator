#' generate_horizontal_relations
#' 
#' Generate relations between random individuals.
#'
#' @param settings relations_settings object
#'
#' @return relations data.frame (every row contains one relation)
#'
#' @export
generate_horizontal_relations <- function(settings) {

  population_by_unit <- split(settings@population, settings@population$unit)
  
  relations_by_unit <- lapply(
    population_by_unit,
    function(population) {
  
      humans <- population$id
      
      from_index <- rep(101:(length(humans) - 100), each = settings@amount_friends)
      to_index <- floor(
        stats::runif(
          length(from_index), 
          min = from_index - 100, 
          max = from_index + 100
        )
      )
      
      vertical_relations <- tibble::tibble(
        from = humans[from_index], 
        to = humans[to_index],
        unit = population$unit[1],
        type = "friend"
      )
    
    }
  )
  
  all_relations <- do.call(rbind, relations_by_unit)
  
  return(all_relations)

}

#### helper functions ####

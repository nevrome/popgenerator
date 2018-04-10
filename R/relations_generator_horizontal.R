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
      
      from_index <- rep(101:length(humans), each = settings@amount_friends)
      
      to_index_begin <- floor(
        stats::runif(
          100, 
          min = 1, 
          max = 1:100 + 100
        )
      )
      to_index_middle <- floor(
        stats::runif(
          length(from_index) - 200, 
          min = 101:(length(from_index) - 100) - 100,
          max = 101:(length(from_index) - 100) + 100
        )
      )
      to_index_end <- floor(
        stats::runif(
          100, 
          min = (length(from_index) - 100):length(from_index) - 100, 
          max = (length(from_index) - 100):length(from_index)
        )
      )
      to_index <- c(to_index_begin, to_index_middle, to_index_end)
      
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

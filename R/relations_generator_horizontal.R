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

  population <- settings@population[order(settings@population$birth_time), ]
  
  population_by_unit <- split(population, population$unit)
  
  relations_by_unit <- lapply(
    population_by_unit,
    function(population) {
  
    humans <- population$id
    
    from <- rep(humans, each = settings@amount_friends)
    to_begin <- floor(
      stats::runif(
        100, 
        min = 0, 
        max = humans[1:100] + 100
      )
    )
    to_middle <- floor(
      stats::runif(
        length(from) - 200, 
        min = humans[101:(length(humans) - 100)] - 100, 
        max = humans[101:(length(humans) - 100)] + 100
      )
    )
    to_end <- floor(
      stats::runif(
        100, 
        min = humans[(length(humans) - 100):(length(humans))] - 100, 
        max = humans[(length(humans) - 100):(length(humans))]
      )
    )
    to <- c(to_begin, to_middle, to_end)
    
    vertical_relations <- tibble::tibble(
      from = from, 
      to = to, 
      type = "friend"
    )
    
    }
  )
  
  all_relations <- do.call(rbind, relations_by_unit)
  
  return(all_relations)

}

#### helper functions ####

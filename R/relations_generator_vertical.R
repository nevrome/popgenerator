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
  humans <- population$id
  
  to <- rep(humans[-(1:100)], each = 2)
  from <- round(stats::runif(length(to), min = to - 100, max = to - 1), 0)
  
  vertical_relations <- tibble::tibble(
    from = from, 
    to = to, 
    type = "child_of"
  )
  
  return(vertical_relations)
  
}

#### helper functions ####

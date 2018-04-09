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
  humans <- population$id
  
  from <- rep(humans, each = settings@amount_friends)
  to_begin <- round(
    stats::runif(
      100, 
      min = 0, 
      max = humans[1:100] + 100
    ),
    0
  )
  to_middle <- round(
    stats::runif(
      length(from) - 200, 
      min = humans[101:(length(humans) - 100)] - 100, 
      max = humans[101:(length(humans) - 100)] + 100
    ),
    0
  )
  to_end <- round(
    stats::runif(
      100, 
      min = humans[(length(humans) - 100):(length(humans))] - 100, 
      max = humans[(length(humans) - 100):(length(humans))]
    ),
    0
  )
  to <- c(to_begin, to_middle, to_end)
  
  vertical_relations <- tibble::tibble(
    from = from, 
    to = to, 
    type = "friend"
  )
  
  return(vertical_relations)

}

#### helper functions ####

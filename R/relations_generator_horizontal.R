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
      
      shift <- rep(      
        get_average_index_shift_vector(
          population$birth_time, 50, n = 5, sides = 2
        )[101:(length(humans) - 100)],
        each = 2
      )
      
      to_index <- floor(
        stats::runif(
          length(from_index), 
          min = from_index - shift, 
          max = from_index + shift
        )
      )
      
      horizontal_relations <- tibble::tibble(
        from = humans[from_index], 
        to = humans[to_index],
        unit = population$unit[1],
        type = "friend"
      )
      
      return(horizontal_relations)
    }
  )
  
  all_relations <- do.call(rbind, relations_by_unit)
  
  return(all_relations)

}

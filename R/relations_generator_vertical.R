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
      
      from_index <- rep(1:length(humans), each = 2)
      
      downshift_1 <- rep(      
        get_average_index_shift_vector(
          population$birth_time, 40, n = 5, sides = 1
        ),
        each = 2
      )
      downshift_1 <- rlang::prepend(downshift_1, rep(downshift_1[1], 2))
      
      downshift_2 <- rep(      
        get_average_index_shift_vector(
          population$birth_time, 15, n = 5, sides = 1
        ),
        each = 2
      )
      downshift_2 <- rlang::prepend(downshift_2, rep(downshift_2[1], 2))

      to_index <- floor(
        stats::runif(
          length(from_index), 
          min = from_index - downshift_1, 
          max = from_index - downshift_2
        )
      )
      to_index[to_index < 1] <- NA
      
      vertical_relations <- tibble::tibble(
        from = humans[from_index], 
        to = humans[to_index], 
        unit = population$unit[1],
        type = "child_of"
      )
      vertical_relations <- vertical_relations[!is.na(vertical_relations$to), ]
      
      return(vertical_relations)
    }
  )

  all_relations <- do.call(rbind, relations_by_unit)
  
  return(all_relations)
  
}

#### helper functions ####

get_average_index_shift_vector <- function(x, index_shift, n = 5, sides = 1) {
  ais <- index_shift / diff(x)
  ais[is.infinite(ais)] <- index_shift
  ais <- moving_average(
    ais, n = n,  sides = sides
  )
  ais <- as.vector(ais)
  ais[is.na(ais)] <- index_shift
  ais <- ceiling(ais)
  
  return(ais)
}

moving_average <- function(x, n = 5, sides = 1) {
  stats::filter(x, rep(1/n, n), sides = sides)
}


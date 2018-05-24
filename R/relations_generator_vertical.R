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

  # relations are created within each unit
  population_by_unit <- split(settings@population, settings@population$unit)
  relations_by_unit <- lapply(
    population_by_unit,
    function(population) {
      
      # relations are between nodes/humans -- these are identified with an id
      humans <- population$id
      
      # everybody needs two parents, so we need a index vector with every index appearing twice
      from_index <- rep(1:length(humans), each = 2)
      
      # parents are randomly sampled from an index range that defines a number of persons that are
      # approximately in the right age to have children
      # to find this index range, we have to consider the average distance beween the birth-times of the
      # members of a unit -- this value can change massively over time in case of a changing population size
      
      # downshift_1 is the lower limit of the possible parent index range
      downshift_1 <- rep(      
        get_average_index_shift_vector(
          x = population$birth_time, age_shift = 40, n = 5, sides = 1
        ),
        each = 2
      )
      downshift_1 <- rlang::prepend(downshift_1, rep(downshift_1[1], 2))
      
      # downshift_2 is the upper limit of the possible parent index range
      downshift_2 <- rep(      
        get_average_index_shift_vector(
          x = population$birth_time, age_shift = 15, n = 5, sides = 1
        ),
        each = 2
      )
      downshift_2 <- rlang::prepend(downshift_2, rep(downshift_2[1], 2))

      # the parents are randomly drawn from the index range between downshift_1 and downshift_2
      to_index <- floor(
        stats::runif(
          length(from_index), 
          min = from_index - downshift_1, 
          max = from_index - downshift_2
        )
      )
      from_index <- from_index[!(to_index < 1)]
      to_index <- to_index[!(to_index < 1)]

      # finally the vertical relations are constructed
      vertical_relations <- tibble::tibble(
        from = humans[from_index], 
        to = humans[to_index], 
        unit = population$unit[1],
        type = "child_of"
      )
      
      return(vertical_relations)
    }
  )

  all_relations <- do.call(rbind, relations_by_unit)
  
  return(all_relations)
  
}

#### helper functions ####

get_average_index_shift_vector <- function(x, age_shift, n = 5, sides = 1) {
  ais <- age_shift / diff(x)
  ais[is.infinite(ais)] <- age_shift
  ais <- moving_average(
    ais, n = n,  sides = sides
  )
  ais <- as.vector(ais)
  ais[is.na(ais)] <- age_shift
  ais <- ceiling(ais)
  
  return(ais)
}

moving_average <- function(x, n = 5, sides = 1) {
  stats::filter(x, rep(1/n, n), sides = sides)
}


#' generate_population
#'
#' Generate a population by creating the individual unit populations
#' and merge them
#'
#' @param settings population_settings object
#'
#' @return data.frame with population (one row for each individual)
#'
#' @export
generate_population <- function(settings) {
  
  unit_settings <- init_unit_settings(settings)
  
  population <- do.call(rbind, lapply(
    unit_settings, function(x) {
      unit <- generate_unit(x)
      unit$unit <- x@unit_name
      return(unit)
    }
  ))
  
  # order by birth_time
  population <- population[order(population$birth_time), ]
  # add id column
  rownames(population) <- population$id <- 1:nrow(population)
  
  return(population)
}

#' generate_unit
#'
#' Generate an initial population based on an populations settings
#' object
#'
#' @param settings unit_settings object
#'
#' @return data.frame with population (one row for each individual)
#'
#' @export
generate_unit <- function(settings) {
  
  # get total amount of human-year combinations necessary
  human_year_combinations <- get_number_human_year_combinations(settings)
  # get average life span based on age_distribution_function
  average_life_span <- get_human_average_life_span(settings)
  # calculate necessary number of humans to satisfy human_year_combinations
  # with humans of said life span
  number_of_humans <- human_year_combinations / average_life_span  
  # get_birth starting times of birth windows given said lifespan
  birth_windows <- get_birth_windows(average_life_span, settings)
  # get amount of humans necessary per year in every birth_window
  human_year_birth_window <- get_number_human_year_combinations_birth_window(
    birth_windows, settings
  )
  # get number of humans per birth_window
  humans_per_birth_window <- get_number_humans_per_birth_window(
    number_of_humans, human_year_birth_window
  )
  # generate humans for every birth_window  
  generated_humans_raw <- generate_humans_per_birth_window(
    birth_windows, humans_per_birth_window, settings
  )
  # merge birth_window wise humans lists into a single data.frame
  generated_humans <- do.call(rbind.data.frame, generated_humans_raw)

  return(generated_humans)
  
}

#### helper functions start ####

get_number_human_year_combinations <- function(settings) {
  stats::integrate(
    Vectorize(settings@unit_size_function), 
    lower = min(settings@time) - max(settings@age_range), 
    upper = max(settings@time) + max(settings@age_range),
    subdivisions = 1000,
    rel.tol = 1
  )$value
}

get_human_average_life_span <- function(settings) {
  unnormal_prop <- settings@age_distribution_function(settings@age_range) 
  normal_prop <- (unnormal_prop / sum(unnormal_prop))
  sum(settings@age_range * normal_prop)
}

get_birth_windows <- function(average_life_span, settings) {
  seq(
    min(settings@time) - max(settings@age_range),
    max(settings@time) + max(settings@age_range),
    abs(
      (max(settings@time) + max(settings@age_range)) - 
        (min(settings@time) - max(settings@age_range))
    ) / average_life_span
  )
}

get_number_human_year_combinations_birth_window <- function(birth_windows, settings) {
  mapply(
    function(x, y) {
      stats::integrate(
        Vectorize(settings@unit_size_function), 
        lower = x, 
        upper = y,
        subdivisions = 1000,
        rel.tol = 1
      )$value
    },
    x = birth_windows[-length(birth_windows)],
    y = birth_windows[-1]
  )
}

get_number_humans_per_birth_window <- function(
  number_of_humans, human_year_birth_window
  ) {
  number_of_humans * (human_year_birth_window/sum(human_year_birth_window))
}

generate_humans_per_birth_window <- function(
  birth_windows, humans_per_birth_window, settings
) {
  mapply(
    function(start, stop, n, settings) {
      generate_humans(start, stop, n, settings)
    },
    start = birth_windows[-length(birth_windows)],
    stop = birth_windows[-1],
    n = humans_per_birth_window,
    MoreArgs = list(settings = settings),
    SIMPLIFY = FALSE
  )
}

#### helper functions end ####

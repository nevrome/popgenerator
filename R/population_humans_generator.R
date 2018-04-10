#### main function ####

#' generate_humans
#'
#' @param start integer start time 
#' @param stop integer stop time 
#' @param n integer amount of humans to generate
#' @param settings population_settings object
#' 
#' @return data.frame with population (one row for each individual)
#'
#' @export
generate_humans <- function(
  start,
  stop,
  n,
  settings
  ) {
  
  # calc length of one generation
  generation_length <- abs(stop - start)
  
  # generate age, lifetime and unit attribution
  age <- get_attribute(
    n, 
    settings@age_distribution_function, 
    settings@age_range
  )
  birth_time <- get_birth_time(
    start,
    generation_length,
    age
  )
  death_time <- get_death_time(
    birth_time, 
    age
  )
  unit <- get_unit(
    age, 
    settings
  )

  # combine vectors into list
  list(
    age = age,
    birth_time = birth_time,
    death_time = death_time,
    unit = unit
  )
}

#### helper functions ####

get_current_age <- function(start_age, ages) {
  if (is.na(start_age)) {ages/2} else {start_age}
}

get_birth_time <- function(start, generation_length, age) {
  round(
    stats::runif(
      length(age),
      min = start - generation_length/2,
      max = start + generation_length/2
    ),
    0
  )
}

get_death_time <- function(birth_time, age) {
  birth_time + age
}

get_unit <- function(age, settings) {
  floor(
    stats::runif(
      length(age),
      min = 1,
      max = settings@units_amount + 1
    )
  )
}


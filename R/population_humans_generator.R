#### main function ####

#' generate_humans
#'
#' @param t integer time
#' @param n integer amount of humans to generate
#' @param start_id integer id of first generated human. default = 1
#' @param start_age integer current age of humans. default: NA (current ages are generated)
#' @param settings population_settings object
#' @param unit_vector integer vector of unit numbers the new humans can be attributed to 
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
  
  generation_length <- abs(stop - start)
  
  # generate age and lifetime
  age <- get_attribute(
    mean(c(start, stop)), 
    n, 
    settings@age_distribution_function, settings@age_range
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
  
  #unit <- get_attribute(t, n, settings@unit_distribution_function, unit_vector)

  # combine info into data.frame
  list(
    age = age,
    birth_time = birth_time,
    death_time = death_time
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


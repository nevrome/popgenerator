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
  t,
  n,
  start_id = 1,
  start_age = NA,
  settings,
  unit_vector
  ) {
  
  # get death ages of humans
  ages <- get_attribute(t, n, settings@age_distribution_function, settings@age_range)

  # generate humans
  tibble::tibble(
    id =          get_id_range(start_id, n),
    current_age = get_current_age(start_age, ages),
    death_age =   get_death_age(ages),
    dead =        FALSE,
    birth_time =  get_birth_time(t, .data$current_age),
    death_time =  get_death_time(t, .data$death_age, .data$current_age),
    sex =         get_attribute(t, n, settings@sex_distribution_function, settings@sex_range),
    unit =        get_attribute(t, n, settings@unit_distribution_function, unit_vector),
    unit_dead =   FALSE
  )
}

#### helper functions ####

get_id_range <- function(start_id, n) {
  start_id:(start_id + n - 1)
}

get_current_age <- function(start_age, ages) {
  if (is.na(start_age)) {ages/2} else {start_age}
}

get_death_age <- function(ages) {
  ages
}

get_birth_time <- function(t, current_age) {
  as.integer(t - current_age)
}

get_death_time <- function(t, death_age, current_age) {
  as.integer(t + (death_age - current_age))
}

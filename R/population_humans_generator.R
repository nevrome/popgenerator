#### main function ####

#' generate
#'
#' @param t double time
#' @param n integer amount of humans to generate
#' @param start_id integer id of first generated human. default = 1
#' @param start_age integer current age of humans. default: NA (current ages are generated)
#' @param settings test
#' @param unit_vector test
#'
#' @return tibble with humans
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
  ages <- get_attribute(t, n, settings@age_distribution_function, settings@age_range)

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
  ) %>%
    return()
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

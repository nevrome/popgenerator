#### main function ####

#' simulate growth of a population
#'
#' @param humans tibble input population
#' @param time vector time steps
#' @param settings test
#'
#' @return tibble output population
#'
#' @export
simulate_growth <- function(
  humans,
  time,
  settings
) {

  unit_counter <- humans %>% get_last_established_unit()

  pb <- utils::txtProgressBar(style = 3)
  for (t in time[-1]) {
    utils::setTxtProgressBar(pb, t/length(time))

    humans %<>% age()
    humans %<>% find_and_realize_deaths()
    necessary_births <- humans %>% calculate_amount_of_necessary_births(t)

    # stop if population is big enough
    if (necessary_births <= 0) next

    # units handling
    units <- humans %>% get_currently_alive_units()
    units_target_amount <- unit_amount(t)
    if (length(units) > units_target_amount) {
      new_unit_vector <- shrink_unit_vector(units, units_target_amount)
      humans %<>% realize_unit_deaths(new_unit_vector)
    } else if (length(units) < units_target_amount) {
      difference <- determine_amount_missing_units(units, units_target_amount)
      new_unit_vector <- enlarge_unit_vector(units, unit_counter, difference)
      unit_counter <- unit_counter + difference
    } else {
      new_unit_vector <- units
    }

    # generate and add new humans
    humans %<>% rbind(
      generate_humans(
        t = t,
        n = necessary_births,
        start_id = max(humans$id + 1),
        start_age = 0,
        settings,
        unit_vector = new_unit_vector
      )
    )
  }
  close(pb)

  return(humans)
}

#### helper functions ####

age <- function(humans) {
  humans[!humans$dead, ] %<>%
    dplyr::mutate(
      current_age = as.integer(.data$current_age + 1)
    )
  return(humans)
}

find_and_realize_deaths <- function(humans) {
  humans[!humans$dead, ] %<>%
    dplyr::mutate(
      dead = .data$current_age >= .data$death_age
    )
  return(humans)
}

realize_unit_deaths <- function(humans, new_unit_vector) {
  humans[!humans$unit_dead, ] %<>%
    dplyr::mutate(
      unit_dead = ifelse(.data$unit %in% new_unit_vector, FALSE, TRUE)
    )
  return(humans)
}

get_amount_of_living_humans <- function(humans) {
  sum(!humans$dead)
}

calculate_amount_of_necessary_births <- function(humans, t) {
  round(population_size(t) - get_amount_of_living_humans(humans), 0)
}

get_currently_alive_units <- function(humans) {
  unique(humans$unit[!humans$unit_dead])
}

get_last_established_unit <- function(humans) {
  max(humans$unit)
}

enlarge_unit_vector <- function(units, unit_counter, difference) {
  c(units, (unit_counter + 1):(unit_counter + difference))
}

shrink_unit_vector <- function(units, units_target_amount) {
  resample(units, units_target_amount)
}

determine_amount_missing_units <- function(units, units_target_amount) {
  abs(units_target_amount - length(units))
}

#### main function ####

#' Simulate growth of a population.
#'
#' @param humans tibble input population
#' @param settings population_settings object
#'
#' @return data.frame with population (one row for each individual)
#'
#' @export
simulate_growth <- function(humans, settings) {

  # get number of last unit (to allow incrementing this number) 
  unit_counter <- get_last_established_unit(humans)

  pb <- utils::txtProgressBar(style = 3)
  # loop through all timesteps except the initial one
  for (t in settings@time[-1]) {
    utils::setTxtProgressBar(pb, t/length(settings@time))

    # let all humans age and die accordingly
    humans <- age(humans)
    humans <- find_and_realize_deaths(humans)
    
    # calculate necessary amount of births to adjust the population 
    # size to the defined population development
    necessary_births <- calculate_amount_of_necessary_births(
      humans, t, settings
    )

    # stop if population is big enough and no new humans have to 
    # be generated
    if (necessary_births <= 0) next

    # determine amount of units and how many units should exist
    units <- get_currently_alive_units(humans)
    units_target_amount <- settings@unit_amount_function(t)
    
    # if too many units exist: get rid of some random units
    if (length(units) > units_target_amount) {
      new_unit_vector <- shrink_unit_vector(units, units_target_amount)
      humans %<>% realize_unit_deaths(new_unit_vector)
    # if not enough units exist: create new units  
    } else if (length(units) < units_target_amount) {
      difference <- determine_amount_missing_units(units, units_target_amount)
      new_unit_vector <- enlarge_unit_vector(units, unit_counter, difference)
      unit_counter <- unit_counter + difference
    # if amount of units correct: do nothing
    } else {
      new_unit_vector <- units
    }

    # generate new humans
    new_humans <- generate_humans(
      t = t,
      n = necessary_births,
      start_id = max(humans$id + 1),
      start_age = 0,
      settings,
      unit_vector = new_unit_vector
    )
    
    # add new humans to general humans list
    humans <- data.table::rbindlist(
        l = list(humans, new_humans),
        use.names = FALSE,
        fill = FALSE,
        idcol = NULL
      )
    
  }
  close(pb)

  return(as.data.frame(humans))
}

#### helper functions ####

age <- function(humans) {
  data.table::set(humans, j = 2L, value = humans$current_age + 1)
  return(humans)
}

find_and_realize_deaths <- function(humans) {
  data.table::set(humans, j = "dead", value = humans$current_age >= humans$death_age)
  return(humans)
}

realize_unit_deaths <- function(humans, new_unit_vector) {
  humans[!humans$unit_dead, ]$unit_dead <- 
    !(humans[!humans$unit_dead, ]$unit %in% new_unit_vector)
  return(humans)
}

get_amount_of_living_humans <- function(humans) {
  sum(!humans$dead)
}

calculate_amount_of_necessary_births <- function(humans, t, settings) {
  round(settings@population_size_function(t) - get_amount_of_living_humans(humans), 0)
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

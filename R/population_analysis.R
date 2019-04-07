#' count living humans per timestep in population
#'
#' @param humans tibble population
#' @param time vector timesteps
#'
#' @return tibble
#'
#' @export
count_living_humans_over_time <- function(humans, time) {
  sapply(
    time, function(x, humans) {
      sum(humans$birth_time <= x & x <= humans$death_time)
    },
    humans
  )
}

#' count population by living units per timestep in population
#'
#' @param humans tibble population
#' @param time vector timesteps
#'
#' @return tibble
#'
#' @export
count_population_by_living_units_over_time <- function(humans, time) {
  humans_by_unit <- split(humans, humans$unit)
  lapply(
    humans_by_unit, function(humans, time) {
      count_living_humans_over_time(humans, time)
    }, 
    time
  )
}

#' count living units per timestep in population
#'
#' @param humans tibble population
#' @param time vector timesteps
#'
#' @return tibble
#'
#' @export
count_living_units_over_time <- function(humans, time) {
  tibble::tibble(time) %>%
    dplyr::mutate(
      n = time %>% purrr::map_int(
        function(x) {
          humans %>% dplyr::filter(
            .data$birth_time <= x & x <= .data$death_time
          ) %>%
            magrittr::extract2("unit") %>%
            unique %>%
            length %>%
            return()
        }
      )
    ) %>% return()
}

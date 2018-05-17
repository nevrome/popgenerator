#' count living humans per timestep in population
#'
#' @param humans tibble population
#' @param time vector timesteps
#'
#' @return tibble
#'
#' @export
count_living_humans_over_time <- function(humans, time) {
  tibble::tibble(time) %>%
    dplyr::mutate(
      n = time %>% purrr::map_int(
        function(x) {
          humans %>% dplyr::filter(
            .data$birth_time <= x & x <= .data$death_time
          ) %>%
            nrow() %>%
            return()
        }
      )
    ) %>% return()
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

#' count population by living units per timestep in population
#'
#' @param humans tibble population
#' @param time vector timesteps
#'
#' @return tibble
#'
#' @export
count_population_by_living_units_over_time <- function(humans, time) {
  expand.grid(time = time, unit = sort(unique(pop$unit))) %>%
    dplyr::mutate(
      n = purrr::map2_int(
        .data$time, .data$unit,
        function(x, y) {
          humans %>% dplyr::filter(
            .data$birth_time <= x & x <= .data$death_time,
            .data$unit == y
          ) %>%
            nrow() %>%
            return()
        }
      )
    ) %>% return()
}

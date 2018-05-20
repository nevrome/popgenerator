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
  expand.grid(time = time, unit = sort(unique(humans$unit))) %>%
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


#' calculate_all_idea_proportions_over_time
#'
#' @param x test
#'
#' @return test
#' 
#' @export
calculate_all_idea_proportions_over_time <- function(x) {
  x$idea_proportions <- pbapply::pblapply(
    x$model_id, 
    FUN = calculate_idea_proportions_over_time,
    x
    #cl = 4
  )
  return(x)
}

#' calculate_idea_proportions_over_time
#'
#' @param id test
#' @param x test
#'
#' @return test
#' 
#' @export
calculate_idea_proportions_over_time <- function(id, x) {
  
  timesteps <- x$timeframe[[id]]
  cremation <- x$simulation_results[[id]]$notes_per_idea$cremation
  inhumation <- x$simulation_results[[id]]$notes_per_idea$inhumation
  pop <- x$populations[[id]]
  complete_pop <- count_living_humans_over_time(pop, timesteps)$n
  
  proportions <- tibble::tibble(
    timesteps = timesteps,
    crem = count_living_humans_over_time(pop[cremation, ], timesteps)$n,
    inhu = count_living_humans_over_time(pop[inhumation, ], timesteps)$n
  ) %>%
    dplyr::mutate(
      not_involved = complete_pop - (.data$crem + .data$inhu)
    ) %>%
    dplyr::mutate(
      crem = .data$crem / complete_pop,
      inhu = .data$inhu / complete_pop,
      not_involved = .data$not_involved / complete_pop
    ) %>%
    tidyr::gather(
      "variant", "individuals_with_variant", -.data$timesteps
    )
  
  return(proportions)
}


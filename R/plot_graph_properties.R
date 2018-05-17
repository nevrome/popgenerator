#' plot_population_development
#'
#' @param pop population data.frame
#' @param time time
#'
#' @return ggplot2 plot object
#' 
#' @export
plot_population_development <- function(pop, time = c()) {
  if (length(time) < 2) {
    time <- min(pop$birth_time):max(pop$death_time)
  }
  population_over_time <- count_living_humans_over_time(pop, time)

  population_development_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = population_over_time,
      ggplot2::aes_string(x = "time", y = "n"),
      color = "red"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, max(time), by = 100))
  
  return(population_development_plot)
}

#' plot_relations_development
#'
#' @param pop population data.frame
#' @param rel relations data.frame
#' @param time time
#'
#' @return ggplot2 plot object
#' 
#' @export
plot_relations_development <- function(pop, rel, time = c()) {
  if (length(time) < 2) {
    time <- min(pop$birth_time):max(pop$death_time)
  }
  rel_with_age <- rel %>% dplyr::left_join(
    pop[, c("id", "birth_time", "death_time")], by = c("from" = "id")
  ) %>% dplyr::left_join(
    pop[, c("id", "birth_time", "death_time")], by = c("to" = "id"),
    suffix = c("_from", "_to")
  )
  
  relations_over_time <- tibble::tibble(time) %>%
    dplyr::mutate(
      n = time %>% purrr::map_int(
        function(x) {
          rel_with_age %>% dplyr::filter(
            .data$birth_time_from <= x & x <= .data$death_time_from,
            .data$birth_time_to <= x & x <= .data$death_time_to
          ) %>%
            nrow() %>%
            return()
        }
      )
    )

  relations_development_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = relations_over_time,
      ggplot2::aes_string(x = "time", y = "n"),
      color = "red"
    ) +
    ggplot2::scale_x_continuous(breaks = seq(0, max(time), by = 100))
  
  return(relations_development_plot)
}


# population_expected <- tibble::tibble(
#   time = time,
#   n = test$population_settings[[1]]@population_size_function(time)
# )

# units_real <- pop %>% count_living_units_over_time(time)



# unit_development_plot <- ggplot() +
#   geom_line(
#     data = units_real,
#     aes(x = time, y = n),
#     color = "red"
#   )

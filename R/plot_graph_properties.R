#' plot_population_development
#'
#' @param pop population data.frame
#' @param timeframe timeframe
#'
#' @return ggplot2 plot object
#' 
#' @export
plot_population_development <- function(pop, timeframe = c()) {
  if (length(timeframe) < 2) {
    timeframe <- min(pop$birth_time):max(pop$death_time)
  }
  population_over_time <- count_living_humans_over_time(pop, timeframe)
  
  population_development_plot <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = population_over_time,
      ggplot2::aes_string(x = "time", y = "n"),
      color = "red"
    )
  
  return(population_development_plot)
}


# population_expected <- tibble::tibble(
#   time = timeframe,
#   n = test$population_settings[[1]]@population_size_function(time)
# )

# units_real <- pop %>% count_living_units_over_time(timeframe)



# unit_development_plot <- ggplot() +
#   geom_line(
#     data = units_real,
#     aes(x = time, y = n),
#     color = "red"
#   )

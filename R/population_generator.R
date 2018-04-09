#' generate_population
#'
#' Generate an initial population based on an populations settings
#' object and let it live over the course of the timeframe. That includes
#' death and birth of individual humans. Their units die and rise with
#' them.
#'
#' @param settings population_settings object
#'
#' @return data.frame with population (one row for each individual)
#'
#' @export
generate_population <- function(settings) {

  human_year_combinations <- integrate(
    Vectorize(settings@population_size_function), 
    lower = min(settings@time), 
    upper = max(settings@time)
  )$value
  
  average_life_span <- 22
  
  number_of_humans <- human_year_combinations / average_life_span  
  
  birth_windows <- seq(
    min(settings@time),
    max(settings@time),
    abs(max(settings@time) - min(settings@time)) / average_life_span
  )
  
  human_year_per_birth_window <- mapply(
    function(x, y) {
      integrate(
        Vectorize(settings@population_size_function), 
        lower = x, 
        upper = y,
        subdivisions = 1000
      )$value
    },
    x = birth_windows[-length(birth_windows)],
    y = birth_windows[-1]
  )
  
  humans_per_birth_window <- number_of_humans * 
    (human_year_per_birth_window/sum(human_year_per_birth_window))
  
  generated_humans_raw <- mapply(
    function(start, stop, n, settings) {generate_humans(start, stop, n, settings)},
    start = birth_windows[-length(birth_windows)],
    stop = birth_windows[-1],
    n = humans_per_birth_window,
    MoreArgs = list(settings = settings),
    SIMPLIFY = FALSE
  )
  
  generated_humans <- do.call(rbind.data.frame, generated_humans_raw)
  
  generated_humans <- generated_humans[order(generated_humans$birth_time), ]
  generated_humans$id <- 1:nrow(generated_humans)
  
  return(generated_humans)
  
  # generate humans now
  
  # # set some start options (situation at the beginning of the timeframe)
  # start_moment <- settings@time[1]
  # intitial_population_size = settings@population_size_function(0)
  # intitial_unit_size = settings@unit_amount_function(0)
  # 
  # # generate the initial population
  # initial_population <- generate_humans(
  #   t = start_moment,
  #   n = intitial_population_size,
  #   start_id = 1,
  #   start_age = NA,
  #   settings = settings,
  #   unit_vector = 1:intitial_unit_size
  # ) %>%
  #   as.data.frame() %>%
  #   dplyr::arrange(
  #     .data$birth_time
  #   ) %>%
  #   dplyr::mutate(
  #     id = 1:intitial_population_size
  #   ) %>%
  #   data.table::as.data.table()
  # 
  # # let the initial population live over the course of the timeframe
  # final_population <- simulate_growth(initial_population, settings) %>%
  #   as.data.frame()
  # 
  # return(final_population)

}

#' generate_all_populations
#'
#' Generate populations in populations_grid data.frame based on
#' population_settings objects in column population_settings. 
#'
#' @param x populations_grid data.frame
#'
#' @return populations_grid data.frame with additional column
#' populations
#'
#' @export
generate_all_populations <- function(x) {
  x %>% 
    dplyr::mutate(
      # generate all populations defined in the grid
      populations = pbapply::pblapply(
        .data$population_settings, 
        generate_population#,
        #cl = 4
      )
    )
}

#' init_population_settings
#'
#' Create population_settings object for every row in the 
#' populations_grid data.frame and add it in an additional
#' column.
#'
#' @param x populations_grid data.frame
#'
#' @return populations_grid data.frame with additional column 
#' population_settings
#'
#' @export
init_population_settings <- function(x) { 
  
  # create list of population_settings objects for each row of 
  # the input populations_grid data.frame
  population_settings <- list() 
  for (i in 1:nrow(x)) {
    population_settings[[i]] <- methods::new(
      "population_settings",
      time =                       x$timeframe[[i]],
      population_size_function =   x$population_size_functions[[i]],
      unit_amount_function =       x$unit_amount_functions[[i]],
      age_distribution_function =  x$age_distribution_functions[[i]],
      age_range =                  x$age_ranges[[i]],
      sex_distribution_function =  x$sex_distribution_functions[[i]],
      sex_range =                  x$sex_ranges[[i]],
      unit_distribution_function = x$unit_distribution_functions[[i]]
    )
  }

  # add new list column with population_settings objects to the
  # input grid
  x %>%
    dplyr::mutate(
      population_settings = population_settings
    )

}

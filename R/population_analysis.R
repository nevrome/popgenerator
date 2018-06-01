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

#' calculate_all_idea_proportions_over_time
#'
#' @param x test
#' @param by_unit test
#'
#' @return test
#' 
#' @export
calculate_all_idea_proportions_over_time <- function(x, by_unit = FALSE) {
  x$idea_proportions <- pbapply::pblapply(
    x$model_id, 
    FUN = calculate_idea_proportions_over_time,
    x, by_unit
    #cl = 4
  )
  return(x)
}

#' calculate_idea_proportions_over_time
#'
#' @param id test
#' @param x test
#' @param by_unit test
#'
#' @return test
#' 
#' @export
calculate_idea_proportions_over_time <- function(id, x, by_unit = FALSE) {
  
  pop <- x$populations[[id]]
  timesteps <- x$timeframe[[id]]
  multiplier <- x$multiplier[[id]]
  idea_1_nodes <- x$simulation_results[[id]]$notes_per_idea$idea_1
  idea_2_nodes <- x$simulation_results[[id]]$notes_per_idea$idea_2

  if (by_unit) {
    all_humans <- count_population_by_living_units_over_time(pop, timesteps)
    idea_1_humans <- count_population_by_living_units_over_time(pop[idea_1_nodes, ], timesteps)
    idea_2_humans <- count_population_by_living_units_over_time(pop[idea_2_nodes, ], timesteps)
  } else {
    all_humans <- count_living_humans_over_time(pop, timesteps)
    idea_1_humans <- count_living_humans_over_time(pop[idea_1_nodes, ], timesteps)
    idea_2_humans <- count_living_humans_over_time(pop[idea_2_nodes, ], timesteps)
  }
  
  if (by_unit) {
    all_proportions <- lapply(
      1:length(all_humans), function(unit_id) {
        
        unit_name <- as.character(unit_id) 
        
        if (unit_name %in% names(idea_1_humans) & unit_name %in% names(idea_2_humans)) {
          idea_1 = idea_1_humans[[unit_name]] / (idea_1_humans[[unit_name]] + idea_2_humans[[unit_name]])
          idea_2 = idea_2_humans[[unit_name]] / (idea_1_humans[[unit_name]] + idea_2_humans[[unit_name]])
        } else if (unit_name %in% names(idea_1_humans) & !(unit_name %in% names(idea_2_humans))) {
          idea_1 = 1
          idea_2 = 0
        } else if (unit_name %in% names(idea_2_humans) & !(unit_name %in% names(idea_1_humans))) {
          idea_1 = 0
          idea_2 = 1
        } else {
          stop("empty unit?")
        }
        
        tibble::tibble(
          timestep = timesteps,
          # idea_1 = idea_1_humans[[unit_id]] / all_humans[[unit_id]],
          # idea_2 = idea_2_humans[[unit_id]] / all_humans[[unit_id]],
          # not_involved = (all_humans[[unit_id]] - idea_1_humans[[unit_id]] - idea_2_humans[[unit_id]]) / all_humans[[unit_id]]
          idea_1 = idea_1,
          idea_2 = idea_2
        ) %>%
          tidyr::gather(
            "idea", "proportion", -.data$timestep
          )  %>%
          dplyr::mutate(
            model_id = id,
            multiplier = multiplier,
            region = as.integer(names(all_humans)[[unit_id]])
          ) %>%
          dplyr::select(
            .data$region, .data$timestep, .data$idea, .data$proportion, .data$model_id, .data$multiplier
          )
      }
    ) %>% dplyr::bind_rows()
  } else {
    all_proportions <- tibble::tibble(
      timestep = timesteps,
      # idea_1 = idea_1_humans / all_humans,
      # idea_2 = idea_2_humans / all_humans,
      # not_involved = (all_humans - idea_1_humans - idea_2_humans) / all_humans
      idea_1 = idea_1_humans / (idea_1_humans + idea_2_humans),
      idea_2 = idea_2_humans / (idea_1_humans + idea_2_humans)
    ) %>%
      tidyr::gather(
        "idea", "proportion", -.data$timestep
      ) %>%
      dplyr::mutate(
        model_id = id,
        multiplier = multiplier
      ) %>%
      dplyr::select(
        .data$timestep, .data$idea, .data$proportion, .data$model_id, .data$multiplier
      )
  }

  return(all_proportions)
}


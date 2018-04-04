#### main function ####

#' calculate_relations_weight
#'
#' Add a weight for every relation.
#'
#' @param x a relations data.frame
#' @param settings relations_settings object
#'
#' @return a relations data.frame with the additional column weight
#'
#' @export
calculate_relations_weight <- function(x, settings) {
  x %>%
    dplyr::mutate(
      weight = unlist(purrrlyr::by_row(x, calculate_weight, settings)[[".out"]])
    )
}

#### helper functions ####

calculate_weight <- function(x, settings) {
  dplyr::case_when(
    x$type == "child_of" ~ weight_child_of(x, settings) %>% return,
    x$type == "sexing"   ~ weight_sexing(x, settings) %>% return
  )
}

weight_child_of <- function(x, settings) {
  get_relation_weight(
    mean(x$start_time, x$end_time), 
    1,
    settings@child_of_weight_distribution_function
  ) %>% 
    return()
}

weight_sexing <- function(x, settings) {
  5
}


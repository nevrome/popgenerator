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
  x$weight <- calculate_weight(x, settings)
  return(x)
}

#### helper functions ####

calculate_weight <- function(x, settings) {
  dplyr::case_when(
    x$type == "child_of" ~ weight_child_of(x, settings),
    x$type == "friend"   ~ weight_friend(x, settings)
  )
}

weight_child_of <- function(x, settings) {
  # alternative implementation idea: get_attribute...
  settings@weight_child_of
}

weight_friend <- function(x, settings) {
  settings@weight_friend
}


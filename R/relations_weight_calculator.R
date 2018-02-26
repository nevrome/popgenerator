#' calculate relations weight
#'
#' @param x a relations table
#' @param settings relations_settings object
#'
#' @return huup
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
  1
}

weight_sexing <- function(x, settings) {
  5
}


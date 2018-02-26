#' relations_settings class
#'
#' class to store values that should be passed to the relations generator
#'
#' @slot population test
#' @slot monogamy_probability test
#' @slot start_fertility_age test
#' @slot stop_fertility_age test
#' @slot same_unit_as_child_probability test
#' @slot same_unit_as_partner_probability test
#'
#' @export
setClass(
  Class = "relations_settings",
  slots = c(
    population = "data.frame",
    monogamy_probability = "numeric",
    start_fertility_age = "numeric",
    stop_fertility_age = "numeric",
    same_unit_as_child_probability = "numeric",
    same_unit_as_partner_probability = "numeric"
  )
)

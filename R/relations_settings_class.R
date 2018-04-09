#' relations_settings class
#'
#' class to store values that should be passed to the relations generator
#'
#' @slot population test
#' @slot same_unit_as_child_probability test
#' @slot same_unit_as_partner_probability test
#' @slot child_of_weight_distribution_function test
#' @slot amount_friends test
#' @slot friendship_age_distribution_function test
#'
#' @export
setClass(
  Class = "relations_settings",
  slots = c(
    population = "data.frame",
    same_unit_as_child_probability = "numeric",
    same_unit_as_partner_probability = "numeric",
    child_of_weight_distribution_function = "function",
    amount_friends = "numeric",
    friendship_age_distribution_function = "function"
  )
)

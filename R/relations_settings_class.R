#' relations_settings class
#'
#' class to store values that should be passed to the relations generator
#'
#' @slot population test
#' @slot amount_friends test
#' @slot cross_unit_proportion_child_of test
#' @slot cross_unit_proportion_friend test
#' @slot child_of_weight_distribution_function test
#'
#' @export
setClass(
  Class = "relations_settings",
  slots = c(
    population = "data.frame",
    amount_friends = "numeric",
    cross_unit_proportion_child_of = "numeric",
    cross_unit_proportion_friend = "numeric",
    child_of_weight_distribution_function = "function"
  )
)

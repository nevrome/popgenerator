#' relations_settings class
#'
#' class to store values that should be passed to the relations generator
#'
#' @slot population test
#' @slot amount_friends test
#' @slot unit_interaction_matrix test
#' @slot cross_unit_proportion_child_of test
#' @slot cross_unit_proportion_friend test
#' @slot weight_child_of test
#' @slot weight_friend test
#'
#' @export
setClass(
  Class = "relations_settings",
  slots = c(
    population = "data.frame",
    amount_friends = "numeric",
    unit_interaction_matrix = "matrix",
    cross_unit_proportion_child_of = "numeric",
    cross_unit_proportion_friend = "numeric",
    weight_child_of = "numeric",
    weight_friend = "numeric"
  )
)

#' init_relations_settings
#'
#' Create relations_settings object for every row in the 
#' populations_grid data.frame and add it in an additional
#' column.
#'
#' @param population test
#' @param amount_friends test
#' @param unit_interaction_matrix test
#' @param cross_unit_proportion_child_of test
#' @param cross_unit_proportion_friend test
#' @param weight_child_of test
#' @param weight_friend test
#'
#' @return populations_grid data.frame with additional column 
#' relations_settings
#'
#' @export
init_relations_settings <- function(    
  population,
  amount_friends,
  unit_interaction_matrix,
  cross_unit_proportion_child_of,
  cross_unit_proportion_friend,
  weight_child_of,
  weight_friend
) { 
  
  relations_settings <- methods::new(
    "relations_settings",
    population =                     population,
    amount_friends =                 amount_friends,
    unit_interaction_matrix =        unit_interaction_matrix,
    cross_unit_proportion_child_of = cross_unit_proportion_child_of,
    cross_unit_proportion_friend =   cross_unit_proportion_friend,
    weight_child_of =                weight_child_of,
    weight_friend =                  weight_friend
  )

  return(relations_settings)
}

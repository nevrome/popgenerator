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
#' @param x populations_grid data.frame
#' @param populations populations list
#'
#' @return populations_grid data.frame with additional column 
#' relations_settings
#'
#' @export
init_relations_settings <- function(x, populations) { 
  
  relations_settings <- list() 
  for (i in 1:nrow(x)) {
    relations_settings[[i]] <- methods::new(
      "relations_settings",
      population =                     populations[[i]],
      amount_friends =                 x$amounts_friends[[i]],
      unit_interaction_matrix =        x$unit_interaction_matrix[[i]],
      cross_unit_proportion_child_of = x$cross_unit_proportion_child_of[[i]],
      cross_unit_proportion_friend =   x$cross_unit_proportion_friend[[i]],
      weight_child_of =                x$weight_child_of[[i]],
      weight_friend =                  x$weight_friend[[i]]
    )
  }
  
  return(relations_settings)
}

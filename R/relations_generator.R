#' generate relations
#'
#' Generate horizontal and vertical relations for a population
#' based on a relations_settings object. Vertical and horizontal
#' relations are created independently and get merged later.
#' Every relationship type has a specific weight. 
#'
#' @param settings relations_settings object
#'
#' @return huup
#'
#' @export
generate_relations <- function(settings) {

  vertical_relations <- generate_vertical_relations(settings)
  horizontal_relations <- generate_horizontal_relations(settings)
  
  all_relations <- rbind(
    vertical_relations,
    horizontal_relations
  )

  all_relations <- modify_relations_cross_unit(all_relations, settings)
  
  all_relations <- calculate_relations_weight(all_relations, settings)
  
  return(all_relations)

}

#' generate_all_relations
#'
#' Generate relations in a populations_grid data.frame based on
#' relations_settings objects in column relations_settings. 
#'
#' @param x populations_grid data.frame
#'
#' @return populations_grid data.frame with additional column
#' relations
#'
#' @export
generate_all_relations <- function(x) {
  x$relations <- pbapply::pblapply(
    x$relations_settings, 
    generate_relations#,
    #cl = 4
  )
  return(x)
}

#' init_relations_settings
#'
#' Create relations_settings object for every row in the 
#' populations_grid data.frame and add it in an additional
#' column.
#'
#' @param x populations_grid data.frame
#'
#' @return populations_grid data.frame with additional column 
#' relations_settings
#'
#' @export
init_relations_settings <- function(x) { 
  
  relations_settings <- list() 
  for (i in 1:nrow(x)) {
    relations_settings[[i]] <- methods::new(
      "relations_settings",
      population =                     x$populations[[i]],
      amount_friends =                 x$amounts_friends[[i]],
      cross_unit_proportion_child_of = x$cross_unit_proportion_child_of[[i]],
      cross_unit_proportion_friend =   x$cross_unit_proportion_friend[[i]],
      weight_child_of =                x$weight_child_of[[i]],
      weight_friend =                  x$weight_friend[[i]]
    )
  }
  
  x$relations_settings <- relations_settings
  
  return(x)
}


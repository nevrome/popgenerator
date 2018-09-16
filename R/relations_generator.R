#' generate_all_relations
#'
#' Generate relations in a populations_grid data.frame based on
#' relations_settings objects in column relations_settings. 
#'
#' @param x relations_settings list
#'
#' @return populations_grid data.frame with additional column
#' relations
#'
#' @export
generate_all_relations <- function(x) {
  
  relations <- parallel::mclapply(
    x, 
    generate_relations,
    mc.cores = parallel::detectCores()
  )
  
  return(relations)
}

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

  #### vertical relations ####
  vertical_relations <- generate_vertical_relations(settings)

  vertical_relations <- modify_relations_cross_unit(
    vertical_relations, 
    settings@cross_unit_proportion_child_of,
    settings@unit_interaction_matrix,
    settings@population
  )
  
  all_relations <- vertical_relations
  
  #### horizontal relations ####  
  if (settings@amount_friends > 0) {
    horizontal_relations <- generate_horizontal_relations(settings)
    horizontal_relations <- modify_relations_cross_unit(
      horizontal_relations, 
      settings@cross_unit_proportion_friend,
      settings@unit_interaction_matrix,
      settings@population
    )
    all_relations <- rbind(
      vertical_relations,
      horizontal_relations
    )
  }
  
  all_relations <- calculate_relations_weight(all_relations, settings)
  
  return(all_relations)
}

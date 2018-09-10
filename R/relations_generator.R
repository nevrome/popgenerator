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

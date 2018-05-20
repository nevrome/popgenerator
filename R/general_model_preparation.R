#' prepare_pops_rels_ideas
#'
#' @param x models_grid data.frame
#'
#' @return modified models_grid with additional columns
#' 
#' @export
prepare_pops_rels_ideas <- function(x) {
  
  message("Create populations")
  x %<>% init_population_settings()
  x %<>% generate_all_populations()
  
  message("Create relations")
  x %<>% init_relations_settings()
  x %<>% generate_all_relations()
  
  message("Create ideas")
  x %<>% init_ideas_settings()
  
  # message("Create graphs")
  # x %<>% generate_all_graphs()
  
  return(x)
}

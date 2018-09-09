#' prepare_and_export_landscapes
#'
#' @param x models_grid data.frame
#' @param dir_path directory path where to store the output files
#' @param cores how many parallel threads should be used
#'
#' @return modified models_grid with additional columns
#' 
#' @export
prepare_and_export_landscapes <- function(x, dir_path, cores = parallel::detectCores()) {
  
  x$cutting_points_for_compuation <- rep(seq(1, ceiling(nrow(x)/cores)), each = cores)[1:nrow(x)] 
  x_cut <- split(x, as.factor(x$cutting_points_for_compuation))
  
  pbapply::pblapply(
    x_cut, function(y){ 
      population_settings <- init_population_settings(y)
      populations <- generate_all_populations(population_settings)
      
      relations_settings <- init_relations_settings(y, populations)
      relations <- generate_all_relations(relations_settings)
      
      ideas_settings <- init_ideas_settings(y, populations)
      
      write_all_models_to_files(
        populations,
        relations,
        ideas_settings,
        y$timeframe,
        y$model_id,
        dir_path
      )
    }
  )
  
  return(x)
}

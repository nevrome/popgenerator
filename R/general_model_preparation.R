#' run_simulation
#'
#' @param x models_grid data.frame
#' @param dir_path directory path where to store the output files
#' @param only_idea_proportions should only the final result file be stored permanently
#' @param cores how many parallel threads should be used
#'
#' @return modified models_grid with additional columns
#' 
#' @export
run_simulation <- function(x, dir_path, only_idea_proportions = TRUE, cores = parallel::detectCores()) {
  
  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  
  x$cutting_points_for_compuation <- rep(seq(1, ceiling(nrow(x)/cores)), each = cores)[1:nrow(x)] 
  x_cut <- split(x, as.factor(x$cutting_points_for_compuation))
  
  pbapply::pblapply(
    x_cut, function(y){ 
      
      # prepare populations
      population_settings <- init_population_settings(y)
      populations <- generate_all_populations(population_settings)
      
      # prepare relations
      relations_settings <- init_relations_settings(y, populations)
      relations <- generate_all_relations(relations_settings)
      
      # prepare ideas
      ideas_settings <- init_ideas_settings(y, populations)
      
      # only if stated explicitly the intermediate result will be stored 
      current_dir_path <- dir_path
      if (only_idea_proportions) {
        path <- file.path(tempdir(), "gluesless")
        if (!dir.exists(path)) {
          dir.create(path)
        }
        current_dir_path <- path
      }
      
      # write prepared data to file system
      write_models_to_files(
        y$model_id,
        populations,
        relations,
        ideas_settings,
        y$timeframe,
        current_dir_path
      )
      
      # run simulation
      simulation_results <- run_gluesless(
        app_path = "../gluesless/build/gluesless",
        input_file_dir = current_dir_path,
        output_file_dir = current_dir_path,
        models_to_run = y$model_id
      )
      
      # calculate idea proportions proxy
      idea_proportions <- calculate_all_idea_proportions_over_time(
        y$model_id,
        populations, 
        y$timeframe, 
        y$model_group, 
        simulation_results,
        by_unit = TRUE
      )
      
      # write idea proportions
      write_idea_proportions(
        y$model_id, 
        idea_proportions, 
        dir_path
      )
      
      # delete temp files after run
      if (only_idea_proportions) {
        unlink(current_dir_path, recursive = TRUE)
      }
      
    }
  )
  
  return(TRUE)
}

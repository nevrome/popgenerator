#' popgenerator
#'
#' @param x models_grid data.frame
#' @param dir_path directory path where to store the output files
#' @param only_idea_proportions should only the final result file be stored permanently
#' @param cores how many parallel threads should be used
#'
#' @return modified models_grid with additional columns
#' 
#' @export
popgenerator <- function(
  # general settings
  timeframe,
  # population settings  
  unit_amount,
  unit_names,
  unit_size_functions,
  age_distribution_function,
  age_range,
  # relations settings
  amounts_friends,
  unit_interaction_matrix,
  cross_unit_proportion_child_of,
  cross_unit_proportion_friend,
  weight_child_of,
  weight_friend,
  # technical settings
  dir_path, 
  only_idea_proportions = TRUE
) {

  if (!dir.exists(dir_path)) {
    dir.create(dir_path)
  }
  
  # prepare populations
  population_settings <- init_population_settings(  
    timeframe,
    unit_amount,
    unit_names,
    unit_size_functions,
    age_distribution_function,
    age_range
  )
  population <- generate_population(population_settings)
  
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

  return(TRUE)
}

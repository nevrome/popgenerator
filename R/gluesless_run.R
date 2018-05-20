#' run_gluesless
#'
#' @param app_path test
#' @param input_file_dir test
#' @param output_file_dir test
#' @param models_to_run test
#'
#' @return a result list
#' 
#' @export
run_gluesless <- function(
  app_path, 
  input_file_dir, 
  output_file_dir,
  models_to_run
) {
  
  pbapply::pblapply(
    models_to_run, function(x) {
      graph_file <- file.path(input_file_dir, paste0(x, "_pajek_graph.paj"))
      ideas_file <- file.path(input_file_dir, paste0(x, "_idea.txt"))
      output_file <- file.path(output_file_dir, paste0(x, "_result.txt"))
      
      message("\n")    
      system2(app_path, args = c(graph_file, ideas_file, output_file))
      message("\n") 
      
      result_list <- list()
      result <- readLines(output_file)
      
      result_list$number_of_iterations <- as.integer(result[which(result == "Number of iterations:") + 1])
      result_list$number_of_remaining_nodes <- 
        as.integer(unlist(strsplit(
          result[which(result == "Number of remaining nodes after this iteration:") + 1], split = " "
        )))
      result_list$notes_per_idea <- list()
      result_list$notes_per_idea$inhumation <- 
        as.integer(unlist(strsplit(
          result[which(result == "inhumation") + 1], split = " "
        )))
      result_list$notes_per_idea$cremation <- 
        as.integer(unlist(strsplit(
          result[which(result == "cremation") + 1], split = " "
        )))
      
      return(result_list)
    }
  )
  
}






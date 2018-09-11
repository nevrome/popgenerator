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
  
  parallel::mclapply(
    models_to_run, function(x) {
      
      graph_file <- file.path(input_file_dir, paste0(
        formatC(x, width = 5, format = "d", flag = "0"), "_pajek_graph.paj")
      )
      ideas_file <- file.path(input_file_dir, paste0(
        formatC(x, width = 5, format = "d", flag = "0"), "_idea.txt")
      )
      output_file <- file.path(output_file_dir, paste0(
        formatC(x, width = 5, format = "d", flag = "0"), "_result.txt")
      )
      
      system2(app_path, args = c("-pi", graph_file, "-ii", ideas_file, "-o", output_file, "-q"))

      result_list <- list()
      result <- readLines(output_file)
      
      result_list$number_of_iterations <- as.integer(result[which(result == "Number of iterations:") + 1])
      result_list$number_of_remaining_nodes <- 
        as.integer(unlist(strsplit(
          result[which(result == "Number of remaining nodes after this iteration:") + 1], split = " "
        )))
      result_list$notes_per_idea <- list()
      result_list$notes_per_idea$idea_2 <- 
        as.integer(unlist(strsplit(
          result[which(result == "idea_2") + 1], split = " "
        )))
      result_list$notes_per_idea$idea_1 <- 
        as.integer(unlist(strsplit(
          result[which(result == "idea_1") + 1], split = " "
        )))
      
      return(result_list)
    },
    mc.cores = parallel::detectCores()
  )
  
}






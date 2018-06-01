#' write_all_models_to_files
#'
#' @param x models_grid data.frame
#' @param dir_path directory path where to store the files
#'
#' @return called for side effect writing to file system
#' 
#' @export
write_all_models_to_files <- function(x, dir_path) {

  pbapply::pblapply(
    1:nrow(x), function(y) {
      write_pajek_for_snap(
        x$relations[[y]], 
        x$populations[[y]], 
        file.path(dir_path, paste0(x$model_id[y], "_pajek_graph.paj"))
      )
      write_ideas(
        x$ideas_settings[[y]], 
        file.path(dir_path, paste0(x$model_id[y], "_idea.txt"))
      )
    }
  )
  
  return(TRUE)
}


#' write_pajek_for_snap
#'
#' @param graph igraph object
#' @param pop population data.frame
#' @param path output file path
#'
#' @return TRUE, called for the side effect of writing to the file system
#'
#' @export
write_pajek_for_snap <- function(rel, pop, path) {
  if (file.exists(path)) {file.remove(path)}
  
  rel_small <- rel %>% dplyr::select(.data$from, .data$to, .data$weight)
  rel_small <- rel_small[stats::complete.cases(rel_small), ]
  
  write.table(rel_small, file = path, sep = " ", row.names = F, col.names = F)
  
  incomplete_pajek <- readLines(path)
  
  incomplete_pajek <- rlang::prepend(
    incomplete_pajek, 
    paste(c(paste("*Vertices", length(pop$id), collapse = " "), pop$id, "*Edges"), collapse = "\n")             
  )
  
  writeLines(incomplete_pajek, path)
  return(TRUE)
}

#' write_ideas
#'
#' @param settings ideas_settings object
#' @param path output file path
#'
#' @return TRUE, called for the side effect of writing to the file system
#'
#' @export
write_ideas <- function(settings, path) {
  
  content <- paste(
    settings@names,
    sapply(idea_distribution_to_starting_nodes(settings), paste, collapse = " "),
    settings@strength,
    sep = ";",
    collapse = "\n"
  )
  
  if (file.exists(path)) {file.remove(path)}
  file.create(path)
  writeLines(content, path, sep = "")
  
  return(TRUE)
  
}

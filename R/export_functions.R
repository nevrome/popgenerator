#' write_models_to_files
#' 
#' @param model_id vector of model ids
#' @param populations list of populations
#' @param relations list of relations
#' @param ideas_settings list of ideas_settings
#' @param timeframe list of timeframes
#' @param dir_path directory path where to store the output files
#'
#' @return called for side effect writing to file system
#' 
#' @export
write_models_to_files <- function(model_id, populations, relations, ideas_settings, timeframe, dir_path) {

  if (stats::var(c(length(populations), length(relations), length(ideas_settings), length(model_id))) != 0) {
    stop("length of lists is not equal")
  }
  
  parallel::mclapply(
    1:length(populations), function(y) {
      
      # write populations table
      write_population_table(
        populations[[y]],
        file.path(dir_path, paste0(model_id[[y]], "_population.csv"))
      )
      
      # write relations graph 
      write_pajek_for_snap(
        relations[[y]], 
        populations[[y]], 
        file.path(dir_path, paste0(model_id[[y]], "_pajek_graph.paj")),
        file.path(dir_path, paste0(model_id[[y]], "_pajek_graph_simple_version.paj"))
      )
      # write idea file
      write_ideas(
        ideas_settings[[y]], 
        file.path(dir_path, paste0(model_id[[y]], "_idea.txt")),
        start_time = timeframe[[y]][1]
      )
      
    },
    mc.cores = parallel::detectCores()
  )
  
  return(TRUE)
}

#' write_idea_proportions
#' 
#' @param model_id model_id 
#' @param idea_proportions idea_proportions list
#' @param dir_path output dir path
#'
#' @return TRUE, called for the side effect of writing to the file system
#'
#' @export
write_idea_proportions <- function(model_id, idea_proportions, dir_path) {
  
  parallel::mclapply(
    1:length(idea_proportions), function(y) {
      path <- file.path(dir_path, paste0(model_id[[y]], "_idea_proportions.csv"))
      if (file.exists(path)) {file.remove(path)}
      utils::write.csv(idea_proportions[[y]], file = path, row.names = FALSE)
    },
    mc.cores = parallel::detectCores()
  )
  
}

#' write_population_table
#'
#' @param pop population data.frame
#' @param path output file path
#'
#' @return TRUE, called for the side effect of writing to the file system
#'
#' @export
write_population_table <- function(pop, path) {
  if (file.exists(path)) {file.remove(path)}
  utils::write.csv(pop, file = path, row.names = FALSE)
}

#' write_pajek_for_snap
#'
#' @param rel relations data.frame
#' @param pop population data.frame
#' @param path output file path
#' @param simple_version_path output file path for reduced outfile file version
#'
#' @return TRUE, called for the side effect of writing to the file system
#'
#' @export
write_pajek_for_snap <- function(rel, pop, path, simple_version_path = NULL) {
  if (file.exists(path)) {file.remove(path)}
  
  rel_small <- rel %>% dplyr::select(.data$from, .data$to, .data$weight)
  rel_small <- rel_small[stats::complete.cases(rel_small), ]
  
  # simple version
  if (!is.null(simple_version_path)) {
    utils::write.table(rel_small, file = simple_version_path, sep = " ", row.names = F, col.names = F)
    incomplete_pajek <- readLines(simple_version_path)
    incomplete_pajek <- rlang::prepend(
      incomplete_pajek, 
      paste(c(paste("*Vertices", length(pop$id), collapse = " "), "*Edges"), collapse = "\n")             
    )
    writeLines(incomplete_pajek, simple_version_path)
  }
  
  # normal version
  utils::write.table(rel_small, file = path, sep = " ", row.names = F, col.names = F)
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
#' @param start_time moment zero in the model context 
#'
#' @return TRUE, called for the side effect of writing to the file system
#'
#' @export
write_ideas <- function(settings, path, start_time) {
  
  content <- paste(
    settings@names,
    sapply(idea_distribution_to_starting_nodes(settings, start_time), paste, collapse = " "),
    settings@strength,
    sep = ";",
    collapse = "\n"
  )
  
  if (file.exists(path)) {file.remove(path)}
  file.create(path)
  writeLines(content, path, sep = "")
  
  return(TRUE)
  
}

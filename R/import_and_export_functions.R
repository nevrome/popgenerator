#' write_pajek_for_snap
#'
#' @param graph igraph object
#' @param pop population data.frame
#' @param path output file path
#'
#' @return TRUE, called for the side effect of writing to the file system
#'
#' @export
write_pajek_for_snap <- function(graph, pop, path) {
  if (file.exists(path)) {file.remove(path)}
  igraph::write_graph(graph, path, format = "pajek")
  incomplete_pajek <- readLines(path)
  incomplete_pajek[2] = paste(c(pop$id, "*Edges"), collapse = "\n")
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
    settings@start_distribution,
    settings@strength,
    sep = ";",
    collapse = "\n"
  )
  
  if (file.exists(path)) {file.remove(path)}
  file.create(path)
  writeLines(content, path)
  
  return(TRUE)
  
}

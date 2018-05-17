#' write_pajek
#'
#' @param graph igraph object
#' @param pop_df population data.frame
#' @param path output file path
#'
#' @return TRUE, called for the side effect of writing to the file system
#'
#' @export
write_pajek <- function(graph, pop_df, path) {
  igraph::write_graph(graph, path, format = "pajek")
  incomplete_pajek <- readLines(path)
  incomplete_pajek[2] = paste(c(pop_df$id, "*Edges"), collapse = "\n")
  writeLines(incomplete_pajek, path)
  return(TRUE)
}


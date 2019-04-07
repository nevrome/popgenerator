#' construct_graph
#'
#' @param rel relations data.frame
#'
#' @return igraph graph object
#' 
#' @export
generate_graph <- function(rel) {
  
  rel_small <- rel %>% dplyr::select(.data$from, .data$to, .data$weight)
  rel_small$to <- as.integer(rel_small$to)
  rel_small <- rel_small[stats::complete.cases(rel_small), ]
  
  g <- igraph::graph_from_data_frame(
    rel_small,
    directed = FALSE
  )
  
  return(g)
}


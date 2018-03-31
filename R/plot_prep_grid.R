#' Plot grid attributes
#'
#' @param x grid
#' @param method test
#'
#' @return plot
#'
#' @export
plot_prep_grid <- function(x, method) {
  list_of_interest <- unique(x[[method]])
  timeframe <- x[["timeframe"]]
  
  if (method %in% c(
      "population_size_functions", 
      "unit_amount_functions"
    )) {
    cols <- grDevices::rainbow(length(list_of_interest), 1)
    p <- ggplot2::ggplot() + 
      ggplot2::xlim(c(min(timeframe[[1]]), max(timeframe[[1]])))
    for (i in 1:length(list_of_interest)) {
        p <- p + ggplot2::stat_function(
          ggplot2::aes(y = 0),
          fun = list_of_interest[[i]], 
          colour = cols[i]
        )
    }
    return(p)
  }
  
  
}


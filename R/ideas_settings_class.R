#' ideas_settings class
#'
#' class to store idea definition values
#'
#' @slot names test
#' @slot start_distribution test
#' @slot strength test
#'
#' @export
setClass(
  Class = "ideas_settings",
  slots = c(
    names = "character",
    start_distribution = "numeric",
    strength = "numeric"
  )
)

#' init_ideas_settings
#'
#' Create ideas_settings object for every row in the 
#' populations_grid data.frame and add it in an additional
#' column.
#'
#' @param x populations_grid data.frame
#'
#' @return populations_grid data.frame with additional column 
#' ideas_settings
#'
#' @export
init_ideas_settings <- function(x) { 
  
  ideas_settings <- list() 
  for (i in 1:nrow(x)) {
    ideas_settings[[i]] <- methods::new(
      "ideas_settings",
      names =              x$names[[i]],
      start_distribution = x$start_distribution[[i]],
      strength =           x$strength[[i]]
    )
  }
  
  x$ideas_settings <- ideas_settings
  
  return(x)
}

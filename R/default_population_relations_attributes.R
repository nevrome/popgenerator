#### getter for drawing individual attributes ####

#' get n attributes according to attribute distribution at time
#'
#' @param t double time
#' @param n integer amount
#' @param distribution_function function probability distribution
#' @param range vector value range to query
#'
#' @return vector of attributes
#'
#' @export
get_attribute <- function(t = NA, n = NA, distribution_function, range) {
  if (is.na(t) || is.na(n)) stop()
  # draw sample
  resample(
    range, 
    size = n, 
    replace = TRUE, 
    prob = distribution_function(t)(range)
  )
}


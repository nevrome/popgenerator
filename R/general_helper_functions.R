resample <- function(x, ...) x[sample.int(length(x), ...)]

#' get n attributes according to attribute distribution
#'
#' @param n integer amount
#' @param distribution_function function probability distribution
#' @param range vector value range to query
#'
#' @return vector of attributes
#'
#' @export
get_attribute <- function(n = NA, distribution_function, range) {
  resample(
    range, 
    size = n, 
    replace = TRUE, 
    prob = distribution_function(range)
  )
}

# General roxygen tags
#' @importFrom methods setClass
#' @importFrom rlang .data
#' @importFrom magrittr "%>%"
#' @importFrom magrittr "%<>%"
#' @importFrom magrittr "%$%"

# defining global variables
# ugly solution to avoid magrittr NOTE
# see http://stackoverflow.com/questions/9439256/how-can-i-handle-r-cmd-check-no-visible-binding-for-global-variable-notes-when
globalVariables(".")


#' resample
#' 
#' better base::sample.int() implementation
#' 
#' @param x a vector of one or more elements from which to choose
#' @param ... arguments passed to sample.int() 
#'
#' @return vector of attributes
#'
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

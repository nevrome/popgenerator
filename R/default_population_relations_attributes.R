#### relations attributes ####

#' weight child_of distribution at time
#'
#' @param t double time
#'
#' @return distribution function
#'
#' @export
weight_child_of_distribution <- function(t) {
  function(x) {
    x
  }
}

#' friends age distribution at time
#'
#' @param t double time
#'
#' @return distribution function
#'
#' @export
friendship_age_distribution <- function(t) {
  function(x) {
    stats::dt(x, df = 3)
  }
}

#### getter for drawing individual attributes ####

#' get n relation weights according to relation type weight distribution at time
#'
#' @param t double time
#' @param n integer amount
#' @param distribution_function function probability distribution
#'
#' @return vector of attributes
#'
#' @export
get_relation_weight <- function(t = NA, n = NA, distribution_function) {
  if (is.na(t) || is.na(n)) stop()
  # draw sample
  sample(
    1:100, 
    size = n, 
    replace = TRUE, 
    prob = distribution_function(t)(1:100)
  )/100 %>%
    return
}

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
  sample(
    range, 
    size = n, 
    replace = TRUE, 
    prob = distribution_function(t)(range)
  )
}


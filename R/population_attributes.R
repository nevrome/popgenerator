#### population attributes ####

#' population size at time
#'
#' @param t double time
#'
#' @return integer population size at t
#'
#' @export
population_size <- function(t) {
  round((cos(0.01 * t) + 3) * 100 + 0.2 * t, 0)
}

#' unit amount at time
#'
#' @param t double time
#'
#' @return integer amount of units at t
#'
#' @export
unit_amount <- function(t) {
  round((sin(0.02 * t) + 3) * 2, 0)
}

#### distributions of individual attributes ####

#' population age distribution at time
#'
#' @param t double time
#'
#' @return distribution function
#'
#' @export
age_distribution <- function(t) {
  # currently no time dependend development
  function(x) {
    1 / (1 + 0.0004 * 0.7^(-7*log(x)))
  }
  #plot(0:100, population_age_distribution_at_time(0)(0:100))
}

#' population sex distribution at time
#'
#' @param t double time
#'
#' @return distribution function
#'
#' @export
sex_distribution <- function(t) {
  function(x) {
    rep(1/length(x), length(x))
  }
}

#' population unit distribution at time
#'
#' @param t double time
#'
#' @return distribution function
#'
#' @export
unit_distribution <- function(t) {
  function(x) {
    rep(1/length(x), length(x))
  }
}

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
  sample(range, size = n, replace = TRUE, prob = distribution_function(t)(range))
}


#' generate horizontal relations
#'
#' @param settings test
#'
#' @return huup
#'
#' @export
generate_horizontal_relations <- function(settings) {

  # population <- settings@population
  # 
  # from <- c()
  # to <- c()
  # type <- c()
  # start_time <- c()
  # end_time <- c()
  # 
  # pb <- utils::txtProgressBar(style = 3)
  # for (child in 1:nrow(population)) {
  #   
  #   potential_parents <- get_all_humans_in_child_bearing_age_at_childbirth(settings, child)
  #   monogamous <- is_monogamous(settings)
  #   
  #   
  #   
  #   utils::setTxtProgressBar(pb, child/nrow(population))
  # }
  # close(pb)
  # 
  # horizontal_relations <- tibble::tibble(from, to, type, start_time, end_time)
  # 
  # return(horizontal_relations)

}

#### helper functions ####

get_all_humans_alive_in_timeframe <- function(settings, timeframe) {
  purrr::map(timeframe, function(x){
    get_all_humans_alive_at_time(settings, x)
  }) %>%
    plyr::ldply() %>%
    unique
}

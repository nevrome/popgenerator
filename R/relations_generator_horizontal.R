#' generate horizontal relations
#'
#' @param settings test
#'
#' @return huup
#'
#' @export
generate_horizontal_relations <- function(settings) {

  population <- settings@population

  from <- c()
  to <- c()
  type <- c()
  start_time <- c()
  end_time <- c()
  
  if (settings@amount_friends <= 0) {
    return(
      tibble::tibble(
        from = integer(), 
        to = integer(), 
        type = character(), 
        start_time = integer(), 
        end_time = integer()
      )
    )
  }

  pb <- utils::txtProgressBar(style = 3)
  for (person in 1:nrow(population)) {

    # get all potential friends of an individual human 
    # (humans that are alive within the same timeframe)
    potential_friends <- get_all_humans_alive_in_livetime_of_human(settings, person) %>%
      dplyr::filter(
        .data$id != person
      )

    # determine probability of friendship based on age difference
    # and friendship_age_distribution_function
    potential_friends %<>%
      dplyr::mutate(
        age_difference = abs(.data$birth_time - population$birth_time[person]),
        friend_probability = 
          settings@friendship_age_distribution_function(
            population$birth_time[person]
          )(.data$age_difference)
      )
    
    # select random friends based on friendship probability
    friends <- potential_friends %>% dplyr::sample_n(
      settings@amount_friends,
      weight = potential_friends$friend_probability
    )
    
    # store friendships
    from <- append(from, rep(person, settings@amount_friends))
    to <- append(to, friends$id)
    type <- append(type, rep("friend", settings@amount_friends))
    for (friend in 1:nrow(friends)) {
      start_time <- append(start_time, max(population$birth_time[person], friends$birth_time[friend]))
      end_time <- append(end_time, min(population$death_time[person], friends$death_time[friend]))
    }

    utils::setTxtProgressBar(pb, person/nrow(population))
  }
  close(pb)

  # create friendship data.frame
  horizontal_relations <- tibble::tibble(from, to, type, start_time, end_time)

  return(horizontal_relations)

}

#### helper functions ####

get_all_humans_alive_in_livetime_of_human <- function(settings, id) {
  timeframe <- seq(settings@population$birth_time[id], settings@population$death_time[id], 1)
  get_all_humans_alive_in_timeframe(settings, timeframe)
}

get_all_humans_alive_in_timeframe <- function(settings, timeframe) {
  purrr::map(timeframe, function(x){
    get_all_humans_alive_at_time(settings, x)
  }) %>%
    plyr::ldply() %>%
    unique
}

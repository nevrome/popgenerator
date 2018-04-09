#' generate_horizontal_relations
#' 
#' Generate relations between random individuals.
#'
#' @param settings relations_settings object
#'
#' @return relations data.frame (every row contains one relation)
#'
#' @export
generate_horizontal_relations <- function(settings) {

  population <- settings@population[order(settings@population$birth_time), ]
  humans <- population$id
  
  from <- rep(humans, each = settings@amount_friends)
  to_begin <- round(
    stats::runif(
      100, 
      min = 0, 
      max = humans[1:100] + 100
    ),
    0
  )
  to_middle <- round(
    stats::runif(
      length(from) - 200, 
      min = humans[101:(length(humans) - 100)] - 100, 
      max = humans[101:(length(humans) - 100)] + 100
    ),
    0
  )
  to_end <- round(
    stats::runif(
      100, 
      min = humans[(length(humans) - 100):(length(humans))] - 100, 
      max = humans[(length(humans) - 100):(length(humans))]
    ),
    0
  )
  to <- c(to_begin, to_middle, to_end)
  
  vertical_relations <- tibble::tibble(
    from = from, 
    to = to, 
    type = "friend"
  )
  
  return(vertical_relations)
  
  # population <- settings@population
  # 
  # # estimate size to preallocate vectors
  # estimated_size <- estimate_amount_of_relations(settings)
  # 
  # # empty vectors to store relationships
  # from <- vector("integer", estimated_size)
  # to <- vector("integer", estimated_size)
  # type <- vector("character", estimated_size)
  # start_time <- vector("integer", estimated_size)
  # end_time <- vector("integer", estimated_size)
  # 
  # # if the settings define that humans in general have no friends, then 
  # # give back an empty data.frame
  # if (settings@amount_friends <= 0) {
  #   return(
  #     tibble::tibble(
  #       from = integer(), 
  #       to = integer(), 
  #       type = character(), 
  #       start_time = integer(), 
  #       end_time = integer()
  #     )
  #   )
  # }
  # 
  # pb <- utils::txtProgressBar(style = 3)
  # for (person in 1:nrow(population)) {
  # 
  #   # get all potential friends of an individual human 
  #   # (humans that are alive within the same timeframe)
  #   potential_friends <- get_all_humans_alive_in_livetime_of_human(
  #     settings, person
  #   ) 
  #   
  #   # remove current person of interest
  #   potential_friends <- potential_friends[potential_friends$id != person, ]
  # 
  #   # determine probability of friendship based on age difference
  #   # and friendship_age_distribution_function
  #   potential_friends$age_difference <- abs(
  #     potential_friends$birth_time - population$birth_time[person]
  #   )
  #   potential_friends$friend_probability <- 
  #     settings@friendship_age_distribution_function(
  #       population$birth_time[person]
  #     )(potential_friends$age_difference)
  #   
  #   # select random friends based on friendship probability
  #   friends <- potential_friends %>% dplyr::sample_n(
  #     settings@amount_friends,
  #     weight = potential_friends$friend_probability
  #   )
  #   
  #   # store friendships
  #   start <- (person - 1) * settings@amount_friends + 1
  #   stop <- person * settings@amount_friends
  #   
  #   from[start:stop] <- rep(person, settings@amount_friends)
  #   to[start:stop] <- friends$id
  #   type[start:stop] <- rep("friend", settings@amount_friends)
  #   start_time[start:stop] <- pmax(
  #     population$birth_time[person], 
  #     friends$birth_time
  #   )
  #   end_time[start:stop] <- pmin(
  #     population$death_time[person], friends$death_time
  #   )
  # 
  #   utils::setTxtProgressBar(pb, person/nrow(population))
  # }
  # close(pb)
  # 
  # # create friendship data.frame
  # horizontal_relations <- tibble::tibble(from, to, type, start_time, end_time)
  # 
  # return(horizontal_relations)

}

#### helper functions ####

get_all_humans_alive_in_livetime_of_human <- function(settings, id) {
  start <- settings@population$birth_time[id] 
  stop <- settings@population$death_time[id]
  get_all_humans_alive_in_timeframe(settings, start, stop)
}

get_all_humans_alive_in_timeframe <- function(settings, start, stop) {
  settings@population[
    start <= settings@population$death_time &
    settings@population$birth_time <= stop,
  ]
}

estimate_amount_of_relations <- function(settings) {
  nrow(settings@population) * settings@amount_friends
}

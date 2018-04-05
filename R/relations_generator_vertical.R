#' generate_vertical_relations
#' 
#' Generate relations between parents and children.
#'
#' @param settings relations_settings object
#'
#' @return relations data.frame (every row contains one relation)
#'
#' @export
generate_vertical_relations <- function(settings) {

  # new column to store previous partners for each person
  settings@population$previous_partner <- NA
  
  population <- settings@population

  # empty vectors to store relationships
  from <- c()
  to <- c()
  type <- c()
  start_time <- c()
  end_time <- c()

  pb <- utils::txtProgressBar(style = 3)
  for (child in 1:nrow(population)) {

    potential_parents <- get_all_humans_in_child_bearing_age_at_childbirth(
      settings, child
    )
    monogamous <- is_monogamous(settings)

    # check if there is a potential pair to make a child
    # else the child has no parents
    if (!all(c("male", "female") %in% unique(potential_parents$sex))) {next}

    # randomly select partner1
    partner1_df <- get_parent(settings, potential_parents, child)

    if(
      # check if partner1 already has a previous partner
      !is.na(partner1_df$previous_partner) &&
      # check if the previous partner is in the right age for another child
      partner1_df$previous_partner %in% potential_parents$id &&
      # check if partner1 behaves monogamous
      monogamous
    ) {
      # the previous partner becomes also the parent of this child
      partner2_df <- population[partner1_df$previous_partner, ]
    } else {
      # a new partner is selected
      partner2_df <- get_parent(settings, potential_parents, child, partner1_df)
    }

    # store partner as previous partner
    population$previous_partner[partner1_df$id] <- partner2_df$id
    population$previous_partner[partner2_df$id] <- partner1_df$id

    # establish family relationships
    partner1 <- partner1_df$id
    partner2 <- partner2_df$id

    from <- append(
      from, c(
        partner1,
        partner1,
        partner2
      )
    )
    to <- append(
      to, c(
        partner2,
        child,
        child
      )
    )
    type <- append(
      type, c(
        "sexing",
        "child_of",
        "child_of"
      )
    )
    start_time <- append(
      start_time, c(
        population$birth_time[child] - 1,
        population$birth_time[child],
        population$birth_time[child]
      )
    )
    end_time <- append(
      end_time, c(
        population$birth_time[child] + 2,
        population$birth_time[child] + 14,
        population$birth_time[child] + 14
      )
    )
    utils::setTxtProgressBar(pb, child/nrow(population))
  }
  close(pb)

  vertical_relations <- tibble::tibble(from, to, type, start_time, end_time)

  return(vertical_relations)

}

#### helper functions ####

is_monogamous <- function(settings) {
  stats::runif(1,0,1) <= settings@monogamy_probability
}

get_parent <- function(
  settings, 
  potential_parents, 
  child,
  partner_df = data.frame()
) {
  # if no previous partners available
  if(nrow(partner_df) == 0) {
    # get potential parents in same unit as child
    in_unit <- potential_parents[
      potential_parents$unit == get_parent_unit(settings, child),
    ]
    # if more than one potential parent in same unit available 
    # select one of them randomly
    if(nrow(in_unit) >= 1) {
      return(dplyr::sample_n(in_unit, 1))
    # if not select one potential parent from all potential
    # parents across units 
    } else {
      return(dplyr::sample_n(potential_parents, 1))
    }
  # if previous partners are available  
  } else {
    correct_sex <- potential_parents[potential_parents$sex != partner_df$sex, ]
    in_unit <- correct_sex[
      correct_sex$unit == get_parent_unit(settings, child, partner_df$id),
    ]
    if(nrow(in_unit) >= 1) {
      return(dplyr::sample_n(in_unit, 1))
    } else {
      return(dplyr::sample_n(correct_sex, 1))
    }
  }
}

get_all_humans_in_child_bearing_age_at_childbirth <- function(settings, child) {
  humans_alive_at_childbirth <- get_all_humans_alive_at_childbirth(
    settings, child
  )
  humans_alive_at_childbirth[
    (humans_alive_at_childbirth$birth_time + settings@start_fertility_age) <= 
        settings@population$birth_time[child] &
      settings@population$birth_time[child] <= 
        (humans_alive_at_childbirth$birth_time +  settings@stop_fertility_age),
  ]
}

get_all_humans_alive_at_childbirth <- function(settings, child) {
  get_all_humans_alive_at_time(settings, settings@population$birth_time[child])
}

get_all_humans_alive_at_time <- function(settings, t) {
  settings@population[
      settings@population$birth_time <= t &
      settings@population$death_time >= t,
    ]
}

get_parent_unit <- function(settings, child, partner = NA) {
  if(is.na(partner) & is_same_unit_as_child(settings)) {
    get_unit_of_individual(settings, child)
  } else if (!is.na(partner) & is_same_unit_as_partner(settings)) {
    get_unit_of_individual(settings, partner)
  } else {
    get_available_units_at_childbirth(settings, child) %>%
      resample(., 1)
  }
}

get_unit_of_individual <- function(settings, id) {
  settings@population$unit[id]
}

get_available_units_at_childbirth <- function(settings, child) {
  get_available_units_at_time(settings, settings@population$birth_time[child])
}

get_available_units_at_time <- function(settings, t) {
  get_all_humans_alive_at_time(settings, t)[["unit"]]
}

is_same_unit_as_child <- function(settings) {
  stats::runif(1,0,1) <= settings@same_unit_as_child_probability
}

is_same_unit_as_partner <- function(settings) {
  stats::runif(1,0,1) <= settings@same_unit_as_partner_probability
}

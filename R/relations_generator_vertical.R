#' generate vertical relations
#'
#' @param settings test
#'
#' @return huup
#'
#' @export
generate_vertical_relations <- function(settings) {

  settings@population$previous_partner <- NA
  population <- settings@population

  from <- c()
  to <- c()
  type <- c()
  start_time <- c()
  end_time <- c()

  pb <- utils::txtProgressBar(style = 3)
  for (child in 1:nrow(population)) {

    potential_parents <- get_all_humans_in_child_bearing_age_at_childbirth(settings, child)
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

    population$previous_partner[partner1_df$id] <- partner2_df$id
    population$previous_partner[partner2_df$id] <- partner1_df$id


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
    #label <- append(label, c("❤", "✌", "✌"))
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

  vertical_relations <- tibble::tibble(from, to, type)

  return(vertical_relations)

}

#### helper functions ####

is_monogamous <- function(settings) {
  stats::runif(1,0,1) <= settings@monogamy_probability
}

get_parent <- function(settings, potential_parents, child, partner_df = data.frame()) {
  if(nrow(partner_df) == 0) {
    in_unit <- potential_parents %>%
      dplyr::filter(.data$unit == get_parent_unit(settings, child))
    if(nrow(in_unit) >= 1) {
      in_unit %>% dplyr::sample_n(1) %>% return()
    } else {
      potential_parents %>% dplyr::sample_n(1) %>% return()
    }
  } else {
    correct_sex <- potential_parents %>%
      dplyr::filter(.data$sex != partner_df$sex)
    in_unit <- correct_sex %>%
      dplyr::filter(.data$unit == get_parent_unit(settings, child, partner_df$id))
    if(nrow(in_unit) >= 1) {
      in_unit %>% dplyr::sample_n(1) %>% return()
    } else {
      correct_sex %>% dplyr::sample_n(1) %>% return()
    }
  }
}

get_all_humans_in_child_bearing_age_at_childbirth <- function(settings, child) {
  get_all_humans_alive_at_childbirth(settings, child) %>%
    dplyr::filter(
      (.data$birth_time + settings@start_fertility_age) <= settings@population$birth_time[child],
      settings@population$birth_time[child] <= (.data$birth_time +  settings@stop_fertility_age)
    )
}

get_all_humans_alive_at_childbirth <- function(settings, child) {
  get_all_humans_alive_at_time(settings, settings@population$birth_time[child])
}

get_all_humans_alive_at_time <- function(settings, t) {
  settings@population %>%
    dplyr::filter(
      .data$birth_time <= t,
      .data$death_time >= t
    )
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
  get_all_humans_alive_at_time(settings, t) %>%
    magrittr::extract2("unit")
}

is_same_unit_as_child <- function(settings) {
  stats::runif(1,0,1) <= settings@same_unit_as_child_probability
}

is_same_unit_as_partner <- function(settings) {
  stats::runif(1,0,1) <= settings@same_unit_as_partner_probability
}

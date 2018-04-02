#' generate relations
#'
#' @param settings test
#'
#' @return huup
#'
#' @export
generate_relations <- function(settings) {

  population <- settings@population

  vertical_relations <- settings %>% generate_vertical_relations()
  horizontal_relations <- settings %>% generate_horizontal_relations()
  
  all_relations <- rbind(
    vertical_relations,
    horizontal_relations
  )

  all_relations %<>% deal_with_double_connections()
    
  all_relations %<>% calculate_relations_weight(settings)

  return(all_relations)

}

#' generate_all_relations
#'
#' @param x all_model_populations
#'
#' @return huup
#'
#' @export
generate_all_relations <- function(x) {
  x %<>%
    dplyr::mutate(
      relations = lapply(x$relations_settings, generate_relations)
    )
}

#' init_relations_settings
#'
#' @param x all_model_populations
#'
#' @return huup
#'
#' @export
init_relations_settings <- function(x) { 
  
  relations_settings <- list() 
  for (i in 1:nrow(all_model_populations)) {
    relations_settings[[i]] <- new(
      "relations_settings",
      population =                            x$populations[[i]],
      monogamy_probability =                  x$monogamy_probabilities[[i]],
      start_fertility_age =                   x$start_fertility_ages[[i]],
      stop_fertility_age =                    x$stop_fertility_ages[[i]],
      same_unit_as_child_probability =        x$same_unit_as_child_probabilities[[i]],
      same_unit_as_partner_probability =      x$same_unit_as_partner_probabilities[[i]],
      child_of_weight_distribution_function = x$child_of_weight_distribution_functions[[i]],
      amount_friends =                        x$amounts_friends[[i]],
      friendship_age_distribution_function =  x$friendship_age_distribution_functions[[i]]
    )
  }
  
  x %<>%
    dplyr::mutate(
      relations_settings = relations_settings
    )
  
}


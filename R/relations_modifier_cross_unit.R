#' modify_relations_cross_unit
#'
#' Replace existing relations within units by relations across units.
#'
#' @param relations relations data.frame
#' @param settings relations_settings object
#'
#' @return modified relations data.frame
#' 
#' @export
modify_relations_cross_unit <- function(relations, settings) {
  
  relations_with_to_info <- dplyr::left_join(
    relations,
    settings@population[, !(colnames(settings@population) %in% c("unit"))],
    by = c("to" = "id")
  )
  
  relations_with_to_info <- relations_with_to_info[
    order(relations_with_to_info$birth_time),
  ]
  
  # select different relations by type
  child_of_relations <- relations_with_to_info[
    relations_with_to_info$type == "child_of", 
  ]
  friend_relations <- relations_with_to_info[
    relations_with_to_info$type == "friend", 
  ]
  
  # apply swap partner function with relevant proportion setting
  child_of_relations <- swap_partners(
    relations = child_of_relations,
    amount = calculate_amount_to_replace(
      child_of_relations, 
      settings@cross_unit_proportion_child_of
    )
  )
  friend_relations <- swap_partners(
    relations = friend_relations,
    amount = calculate_amount_to_replace(
      friend_relations, 
      settings@cross_unit_proportion_friend
    )
  )
  
  # combine different relationship types again
  all_relations <- rbind(
    child_of_relations,
    friend_relations
  )
  
  all_relations <- all_relations[
    , !(colnames(all_relations) %in% c("age", "birth_time", "death_time"))
  ]

  return(all_relations)
  
}

#### helper functions ####

swap_partners <- function(relations, amount) {
  
  #deviation_factor <- table(relations$to) %>% max()
  selected_for_swap <- floor(
    stats::runif(
      amount, 
      1, 
      nrow(relations)# - deviation_factor
    )
  )
  
  max_to <- max(relations$to) 
  
  # actual swap: incrementing to vector
  relations$to[selected_for_swap] <- relations$to[selected_for_swap] + 1
  
  # if incrementation causes the to node of some relations to rise 
  # above the number of available nodes then remove this relation
  relations <- relations[!(relations$to > max_to), ]
  
  return(relations)
}

calculate_amount_to_replace <- function(relations, proportion) {
  floor(nrow(relations) * proportion)
}

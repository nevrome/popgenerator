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
    ),
    interaction_matrix = settings@unit_interaction_matrix
  )
  friend_relations <- swap_partners(
    relations = friend_relations,
    amount = calculate_amount_to_replace(
      friend_relations, 
      settings@cross_unit_proportion_friend
    ),
    interaction_matrix = settings@unit_interaction_matrix
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

swap_partners <- function(relations, amount, interaction_matrix) {
  
  selected_for_swap <- floor(
    stats::runif(
      amount, 
      1, 
      nrow(relations)
    )
  )
  
  max_to <- max(relations$to) 
  
  unit_list <- split(relations, relations$unit)
  
  # actual swap
  relations$to[selected_for_swap] <- unname(unlist(lapply(
    selected_for_swap, function(to_index, unit_list) {
      
      # calculate cross unit interaction based on distance matrix
      distance <- as.vector(interaction_matrix[, relations$unit[to_index]])
      distance_accented <- distance^4
      probability_of_interaction <- ifelse(distance_accented != 0, 1/distance_accented, 0)
      
      sample(  
        unname(unlist(lapply(
          unit_list, function(x, to) {
            x$to[x$to > to][1]
          },
          relations$to[to_index]
        ))),
        1,
        prob = probability_of_interaction
      )
    }, 
    unit_list
  )))

  # if incrementation causes the to node of some relations to rise 
  # above the number of available nodes then remove this relation
  relations <- relations[!(relations$to > max_to), ]
  
  return(relations)
}

calculate_amount_to_replace <- function(relations, proportion) {
  floor(nrow(relations) * proportion)
}

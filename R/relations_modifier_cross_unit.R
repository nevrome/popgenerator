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
  
  # select different relations by type
  child_of_relations <- relations[relations$type == "child_of", ]
  friend_relations <- relations[relations$type == "friend", ]
  
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

  return(all_relations)
  
}

#### helper functions ####

swap_partners <- function(relations, amount) {
  
  deviation_factor <- table(relations$to) %>% max()
  selected_for_swap <- floor(
    stats::runif(
      amount, 
      1, 
      nrow(relations) - deviation_factor
    )
  )
  relations <- relations[order(relations$to), ]
    
  first <- relations$to[selected_for_swap]
  first_unit <- relations$unit[selected_for_swap]
  second <- relations$to[selected_for_swap + deviation_factor]
  second_unit <- relations$unit[selected_for_swap + deviation_factor]
  
  relations$to[selected_for_swap] <- second
  relations$unit[selected_for_swap] <- second_unit
  relations$to[selected_for_swap + deviation_factor] <- first
  relations$unit[selected_for_swap + deviation_factor] <- first_unit
  
  return(relations)
}

calculate_amount_to_replace <- function(relations, proportion) {
  floor(nrow(relations) * proportion)
}

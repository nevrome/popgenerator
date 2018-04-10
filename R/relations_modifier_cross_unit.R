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
  
  child_of_relations <- relations[relations$type == "child_of", ]
  friend_relations <- relations[relations$type == "friend", ]
  
  part_parent_child_relations <- 0.05
  part_friend_relations <- 0.1
  
  child_of_relations <- swap_partners(
    child_of_relations,
    calculate_amount_to_replace(child_of_relations, part_parent_child_relations)
  )
  
  friend_relations <- swap_partners(
    friend_relations,
    calculate_amount_to_replace(child_of_relations, part_friend_relations)
  )
  
  all_relations <- rbind(
    child_of_relations,
    friend_relations
  )

  return(all_relations)
  
}

#### helper functions ####

swap_partners <- function(relations, amount) {
  
  selected_for_swap <- floor(stats::runif(amount, 1, nrow(relations)))
  relations <- relations[order(relations$to), ]
  
  first <- relations$to[selected_for_swap]
  first_unit <- relations$unit[selected_for_swap]
  second <- relations$to[selected_for_swap + 1]
  second_unit <- relations$unit[selected_for_swap + 1]
  
  relations$to[selected_for_swap] <- second
  relations$unit[selected_for_swap] <- second_unit
  relations$to[selected_for_swap + 1] <- first
  relations$unit[selected_for_swap + 1] <- first_unit
  
  relations <- relations[order(relations$from), ]
  
  return(relations)
}

calculate_amount_to_replace <- function(relations, proportion) {
  floor(nrow(relations) * proportion)
}

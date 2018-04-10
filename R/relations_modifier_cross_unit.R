modify_relations_cross_unit <- function(relations, settings) {
  
  child_of_relations <- relations[relations$type == "child_of", ]
  friend_relations <- relations[relations$type == "friend", ]
  
  part_parent_child_relations <- 0.05
  
  amount_to_replace <- floor(nrow(child_of_relations) * part_parent_child_relations)
  
  selected_for_swap <- floor(runif(amount_to_replace, 1, nrow(child_of_relations)))

  child_of_relations <- child_of_relations[order(child_of_relations$to), ]
  
  first <- child_of_relations$to[selected_for_swap]
  first_unit <- child_of_relations$unit[selected_for_swap]
  second <- child_of_relations$to[selected_for_swap + 1]
  second_unit <- child_of_relations$unit[selected_for_swap + 1]
  
  child_of_relations$to[selected_for_swap] <- second
  child_of_relations$unit[selected_for_swap] <- second_unit
  child_of_relations$to[selected_for_swap + 1] <- first
  child_of_relations$unit[selected_for_swap + 1] <- first_unit

}

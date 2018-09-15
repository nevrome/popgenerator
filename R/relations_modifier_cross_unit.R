#' modify_relations_cross_unit
#'
#' Replace existing relations within units by relations across units.
#'
#' @param relations relations data.frame
#' @param cross_unit_proportion cross_unit_proportion
#' @param unit_interaction_matrix unit_interaction_matrix
#' @param population population data.frame
#'
#' @return modified relations data.frame
#' 
#' @export
modify_relations_cross_unit <- function(relations, cross_unit_proportion, unit_interaction_matrix, population) {
  
  relations_with_to_info <- dplyr::left_join(
    relations,
    population[, !(colnames(population) %in% c("unit"))],
    by = c("to" = "id")
  )
  
  relations_with_to_info <- relations_with_to_info[
    order(relations_with_to_info$birth_time),
  ]
  
  # apply swap partner function with relevant proportion setting
  new_relations <- swap_partners(
    relations = relations_with_to_info,
    amount = calculate_amount_to_replace(
      relations_with_to_info, 
      cross_unit_proportion
    ),
    interaction_matrix = unit_interaction_matrix
  )
  
  new_relations <- new_relations[
    , !(colnames(new_relations) %in% c("age", "birth_time", "death_time"))
  ]

  return(new_relations)
}



#### helper functions ####

swap_partners <- function(relations, amount, interaction_matrix) {
  
  relations$age_segment <- base::cut(
    relations$birth_time, 
    breaks = seq(
      min(relations$birth_time) - 100, 
      max(relations$birth_time) + 100, 
      50
    ),
    labels = FALSE
  )
  
  y <- relations
  to_by_unit_and_age_segment <- lapply(
    base::split(
      y, 
      y$age_segment
    ), 
    function(x) { 
      lapply(
        base::split(
        x, 
        x$unit
      ),
      function(z) {
        z$to
      }
      )
    }
  )
  
  selected_for_swap <- floor(
    stats::runif(
      amount, 
      1, 
      nrow(relations)
    )
  )
  
  probability_of_interaction_matrix <- ifelse(interaction_matrix != 0, 1/interaction_matrix^4, 0)
  
  #selected_relations <- relations[selected_for_swap, ]
  
  new_partners <- vector(mode = "integer", length = length(selected_for_swap))
  counter <- 1
  for (i in selected_for_swap) {
    age_segment <- as.character(relations$age_segment[i])
    swap_unit <- sample(
      levels(relations$unit),
      1,
      prob = probability_of_interaction_matrix[, relations$unit[i]]
    )
    swap_options <- to_by_unit_and_age_segment[[age_segment]][[swap_unit]]
    if (length(swap_options) > 1) {
      new_partners[counter] <- sample(
        swap_options,
        1
      )
    } else {
      new_partners[counter] <- relations$to[i]
    }
    counter <- counter + 1
  }
  
  relations$to[selected_for_swap] <- new_partners
  
  return(relations)
}

calculate_amount_to_replace <- function(relations, proportion) {
  floor(nrow(relations) * proportion)
}

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
  # horizontal_relations <- settings %>% generate_horizontal_relations()
  
  all_relations <- rbind(
    vertical_relations
  )
  
  all_relations %<>% calculate_relations_weight(settings)

  return(all_relations)

}

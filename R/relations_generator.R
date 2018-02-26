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

  all_relations <- rbind(
    vertical_relations
  )

  return(all_relations)

}
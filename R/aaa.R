resample <- function(x, ...) x[sample.int(length(x), ...)]

#' deal with double connections
#'
#' @param x data.frame
#'
#' @export
deal_with_double_connections <- function(x) {
  x %<>% 
    create_network_comparison_string %>%
    generate_list_of_equality_partners %>%
    add_equality_group_number(x, .)
  
  # get all unique dates
  not_duplicates <- x %>%
    dplyr::filter(
      is.na(.data$duplicate_group)
    )
  
  # get all duplicates
  duplicates <- x %>%
    dplyr::filter(
      !is.na(.data$duplicate_group)
    )
  
  summarised_duplicates <- duplicates %>%
    dplyr::group_by(.data$duplicate_group) %>%
    dplyr::summarise_all(
      .funs = dplyr::funs(compare_and_combine_values(.))
    )
  
  rbind(not_duplicates, summarised_duplicates)
  
 }

#' deal with double values
#'
#' @param x data.frame column
#' 
#'
#' @export
compare_and_combine_values <- function(x) {
  x[1]
}

#' generate relations
#'
#' @param x data.frame
#'
#' @export
create_network_comparison_string <- function(x) {
  x %<>% 
    dplyr::rowwise() %>%
    dplyr::mutate(
      string_rep = paste0(
        min(c(.data$from, .data$to)),
        max(c(.data$from, .data$to))
      )
    ) %>%
    dplyr::ungroup()
  x[["string_rep"]]
}

#' generate_list_of_equality_partners
#'
#' @param x vector
#'
#' @return list of unique partners
#'
#' @keywords internal
generate_list_of_equality_partners <- function(x) {
  x %>% pbapply::pblapply(
    function(y){
      if(!is.na(y)){
        which(y == x)
      } else {
        NA
      }
    }
  ) %>%
    magrittr::extract(sapply(., function(x) {length(x)}) > 1) %>%
    unique() %>%
    return()
}

#' add_equality_group_number
#'
#' @param x data.frame
#' @param partner_list partner list produced by generate_list_of_equality_partners()
#'
#' @return data.frame with additional column duplicate_group
#'
#' @keywords internal
add_equality_group_number <- function(x, partner_list) {
  x$duplicate_group <- NA
  
  if(length(partner_list) > 0) {
    amount_duplicate_groups <- length(partner_list)
    pb <- utils::txtProgressBar(
      min = 0, max = amount_duplicate_groups,
      style = 3,
      width = 50,
      char = "+"
    )
    group_counter = 0
    for (p1 in 1:amount_duplicate_groups) {
      x$duplicate_group[partner_list[[p1]]] <- group_counter
      group_counter <- group_counter + 1
      utils::setTxtProgressBar(pb, p1)
    }
    close(pb)
  }
  
  x$duplicate_group <- as.integer(x$duplicate_group)
  
  return(x)
}

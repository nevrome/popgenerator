number_of_humans <- 40
number_of_steps <- 5
generation_length <- number_of_humans/number_of_steps

humans <- tibble::tibble(
  human_id = 1:number_of_humans,
  age = as.integer(abs(rnorm(number_of_humans, mean = 0, sd = 30))),
  generation_id = as.integer(ceiling(human_id/generation_length)),
  group = generation_id,
  sex = rep(c("male", "female"), number_of_humans/2),
  shape = ifelse(sex == "male", "box", "circle")
)

resample <- function(x, ...) x[sample.int(length(x), ...)]

all_relations <- expand.grid.unique(humans$human_id, humans$human_id)
colnames(all_relations) <- c("from", "to")

# heavy_sexing_2 <- function(x) {
#   relations <- all_relations %>%
#     dplyr::mutate(
#       generation_from = x$generation_id[from],
#       generation_to = x$generation_id[to],
#       generation_difference = as.integer(abs(generation_from - generation_to)),
#       sex_from = x$sex[from],
#       sex_to = x$sex[to],
#       sex_difference = sex_from != sex_to
#     ) %>%
#     dplyr::filter(
#       generation_difference == 0,
#       sex_difference
#     ) %>%
#     dplyr::group_by(
#       from
#     ) %>%
#     dplyr::mutate(
#       randomly_selected = rep(FALSE, n()) %>% inset(sample.int(length(.), 1), TRUE)
#     ) %>%
#     dplyr::ungroup()
#
#
#
#   hu %>%
#     dplyr::filter(
#       !(from %in% .$to)
#     )
#
#   2 %in% .$from
#
# }

heavy_sexing <- function(x) {
  from <- c()
  to <- c()
  for (current_human in 1:nrow(x)) {
    if (current_human %in% c(from, to)) next
    current_generation <- x$generation_id[current_human]
    current_sex <- x$sex[current_human]
    potential_partners <- x$human_id[
      x$generation_id == current_generation &
        x$sex != current_sex &
        !(x$human_id %in% c(from, to))
    ]
    if (length(potential_partners) == 0) next
    partner <- resample(potential_partners, 1)
    from <- append(from, current_human)
    to <- append(to, partner)
  }
  tibble::tibble(from, to, label = "❤", value = 2)
}

humans %>%
  heavy_sexing() -> sexing

sexing %>%
  igraph::graph_from_data_frame(d = .) %>%
  visNetwork::toVisNetworkData() %$%
  visNetwork::visNetwork(nodes, edges) %>%
  visNetwork::visNodes(size = 20)

child_production <- function(x, generation_length, sexing) {
  from <- c()
  to <- c()
  for (parent in 1:nrow(x)) {
    parent_generation_id <- x$generation_id[parent]
    child_generation_id <- parent_generation_id + 1
    if (child_generation_id > max(x$generation_id)) break
    child_pool <- x$human_id[x$generation_id == child_generation_id & !(x$human_id %in% to)]
    if (parent %% generation_length == 0) {
      child <- child_pool
    } else {
      child <- resample(child_pool, sample(ifelse(length(child_pool) < 3, length(child_pool), 3), 1) - 1)
    }
    if (length(child) == 0) next
    from <- append(from, rep(parent, length(child)))
    to <- append(to, child)
    partner <- c(sexing$from[sexing$to == parent], sexing$to[sexing$from == parent])
    from <- append(from, rep(partner, length(child)))
    to <- append(to, child)
  }
  tibble::tibble(from, to, label = "✌", value = 2)
}

humans %>%
  child_production(generation_length, sexing) -> kidding

rbind(
  sexing, kidding
) -> vertical

expand.grid.unique <- function(x, y, include.equals=FALSE) {
  x <- unique(x)
  y <- unique(y)
  g <- function(i) {
    z <- setdiff(y, x[seq_len(i-include.equals)])
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  do.call(rbind, lapply(seq_along(x), g)) %>%
    as.data.frame() %>%
    return()
}

all_relations <- expand.grid.unique(humans$human_id, humans$human_id)
colnames(all_relations) <- c("from", "to")

random_relations <- function(x, all_relations, vertical) {
  dplyr::anti_join(
    all_relations,
    vertical
  ) %>%
    dplyr::mutate(
      generation_from = x$generation_id[from],
      generation_to = x$generation_id[to],
      generation_difference = abs(generation_from - generation_to) + 1,
      value = 1/generation_difference,
      label = "😁"
    ) %>%
    dplyr::select(
      from, to, label, value
    )
}

humans %>%
  random_relations(all_relations, vertical) -> horizontal

rbind(
  vertical, horizontal
) -> network


network %>%
 igraph::graph_from_data_frame(d = ., vertices = humans, directed = FALSE) %>%
  visNetwork::visIgraph(layout = "layout_in_circle")
  # visNetwork::toVisNetworkData() %$%
  # visNetwork::visNetwork(nodes, edges) %>%
  # visNetwork::visIgraph()
  #visNetwork::visHierarchicalLayout(direction = "UD", sortMethod = "directed")

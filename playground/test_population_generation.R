#### setup settings grid ####

# create populations_grid data.frame
all_model_populations <- expand.grid(
  #multiplier = 1:10,
  # general settings
  timeframe = list(
    0:1000
  ),
  # population settings  
  population_size_functions = c(
    function(t) {1000},
    function(t) {round(2000 - 0.95 * t, 0)},
    function(t) {round(100 + 0.95 * t, 0)},
    function(t) {round(0.0019 * (t - 1000)^2 + 100, 0)},
    function(t) {round(-0.0019 * (t - 1000)^2 + 2000, 0)}
  ),
  units_amount = c(
    50,
    100,
    200
  ),
  age_distribution_functions = c(
    function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}
  ),
  age_ranges = list(
    1:100
  ),
  # relations settings
  amounts_friends = list(
    10,
    10,
    50
  ),
  cross_unit_proportion_child_of = list(
    0.02,
    0.05,
    0.1,
    0.5
  ),
  cross_unit_proportion_friend = list(
    0.1,
    0.05,
    0.1,
    0.5
  ),
  weight_child_of = list(
    1,
    3
  ),
  weight_friend = list(
    2
  )
) %>% tibble::as.tibble()

plot_prep_grid(all_model_populations, "population_size_functions")
#plot_prep_grid(all_model_populations, "age_distribution_functions")
#plot_prep_grid(all_model_populations, "friendship_age_distribution_functions")

all_model_populations %<>% init_population_settings()
all_model_populations[1:5,] %>% generate_all_populations() -> test

test %<>% init_relations_settings()
test %>% generate_all_relations() -> test2

test$population_settings[[1]] -> settings 
test$relations_settings[[1]] -> settings

test$populations[[1]] -> pop
save(pop, file = "testresults/pop.RData")
test2$relations[[1]] -> rel
save(rel, file = "testresults/rel.RData")

pop$id
hu <- rel[rel$type == "child_of", ]

hu

pop_small <- pop %>% dplyr::select(id)
rel_small <- rel %>% dplyr::select(from, to, weight)
rel_small$to <- as.integer(rel_small$to )

rel_small <- rel_small[complete.cases(rel_small), ]

g <- igraph::graph_from_data_frame(
  rel_small,
  directed = FALSE
)

igraph::write_graph(g,  "../gluesless/test_data/real_graph_test.paj", format = "pajek")

write_pajek_for_snap(g, pop, "../gluesless/test_data/real_graph.paj")



#### test working with gluesless ####

system("cd testresults && ../../gluesless/build/gluesless ../../gluesless/test_data/real_graph.paj")
result <- readLines("testresults/result.txt")
cremation <- as.integer(unlist(strsplit(result[9], split = " ")))
inhumation <- as.integer(unlist(strsplit(result[11], split = " ")))

load("testresults/pop.RData")

cremation_pop <- pop[cremation, ] %>%
  tibble::as.tibble()

timesteps <- 1:1000

count_in_time <- function(y) {
  purrr::map_int(timesteps, function(x){
    y %>%
      dplyr::mutate(
        is = birth_time <= x & death_time >= x  
      ) %$%
      sum(is)
  })
}

complete_pop <- count_in_time(pop)

simulation_data <- tibble::tibble(
  time = timesteps,
  crem = count_in_time(pop[cremation, ]),
  inhu = count_in_time(pop[inhumation, ]),
  not_involved =  complete_pop - (crem + inhu) 
) %>%
  dplyr::mutate(
    crem = crem / complete_pop,
    inhu = inhu / complete_pop,
    not_involved = not_involved / complete_pop
  ) %>%
  tidyr::gather(
    variant, individuals_with_variant, -time
  )

library(ggplot2)

simulation_data %>%
  ggplot() +
  geom_area(aes(x = time, y = individuals_with_variant, fill = variant, group = variant)) +
  geom_line(aes(x = time, y = individuals_with_variant, group = variant), position = "stack") +
  theme_bw() +
  xlab(expression(paste("t"))) +
  ylab("variants and their occurence in the population [%]")

# pop %>%
#   tibble::as.tibble() %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(
#     years = list(birth_time:death_time)
#   ) %>%
#   dplyr::ungroup()

#### analyse result ####

plot_population_development(pop)

plot_relations_development(pop, rel)

library(cowplot)
cowplot::plot_grid(
  population_development_plot,
  unit_development_plot,
  align = "v",
  nrow = 2,
  labels = "AUTO"
)



#### Plot grid attributes ####

plot_prep_grid <- function(x, method) {
  list_of_interest <- unique(x[[method]])
  timeframe <- x[["timeframe"]]
  
  if (method %in% c(
    "population_size_functions", 
    "unit_amount_functions"
  )) {
    cols <- grDevices::rainbow(length(list_of_interest), 1)
    p <- ggplot2::ggplot() + 
      ggplot2::xlim(c(min(timeframe[[1]]), max(timeframe[[1]])))
    for (i in 1:length(list_of_interest)) {
      p <- p + ggplot2::stat_function(
        ggplot2::aes(y = 0),
        fun = list_of_interest[[i]], 
        colour = cols[i]
      )
    }
    return(p)
  }
  
  if (method %in% c(
    "age_distribution_functions",
    "friendship_age_distribution_functions"
  )) {
    cols <- grDevices::rainbow(length(list_of_interest), 1)
    p <- ggplot2::ggplot() + 
      ggplot2::xlim(c(min(x[["age_ranges"]][[1]]), max(x[["age_ranges"]][[1]]))) +
      ggplot2::ggtitle("only t = 0")
    for (i in 1:length(list_of_interest)) {
      p <- p + ggplot2::stat_function(
        ggplot2::aes(y = 0),
        fun = list_of_interest[[i]](0), 
        colour = cols[i]
      )
    }
    return(p)
  }
  
}



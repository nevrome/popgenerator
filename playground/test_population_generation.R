#### setup settings grid ####

# create populations_grid data.frame
models_grid <- expand.grid(
  #multiplier = 1:10,
  # general settings
  timeframe = list(
    0:2000
  ),
  # population settings  
  population_size_functions = c(
    function(t) {300},
    function(t) {round(0.0005 * (t - 1000)^2 + 100, 0)}
    # function(t) {round(2000 - 0.95 * t, 0)},
    # function(t) {round(100 + 0.95 * t, 0)},
    # function(t) {round(0.0019 * (t - 1000)^2 + 100, 0)},
    # function(t) {round(-0.0019 * (t - 1000)^2 + 2000, 0)}
  ),
  units_amount = c(
    10#,
    # 100,
    # 200
  ),
  age_distribution_functions = c(
    function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}
  ),
  age_ranges = list(
    1:100
  ),
  # relations settings
  amounts_friends = list(
    10#,
    # 10,
    # 50
  ),
  cross_unit_proportion_child_of = list(
    0.02#,
    # 0.05,
    # 0.1,
    # 0.5
  ),
  cross_unit_proportion_friend = list(
    0.1#,
    # 0.05,
    # 0.1,
    # 0.5
  ),
  weight_child_of = list(
    3#,
    # 1
  ),
  weight_friend = list(
    2
  ),
  # ideas settings
  names = list(
    c("idea_1", "idea_2")
  ),
  start_distribution = list(
    c(0.2, 0.8)#,
    #c(0.7, 0.3),
    #c(0.3, 0.7)
  ), 
  strength = list(
    c(1, 1), 
    c(1, 2)#,
    #c(2, 1)
  )
) %>% tibble::as.tibble() %>%
  dplyr::mutate(
    model_id = 1:nrow(.)
  )

models_grid %<>% prepare_pops_rels_ideas()

# models_grid$population_settings[[1]] -> settings 
# models_grid$relations_settings[[1]] -> settings
# models_grid$ideas_settings[[1]] -> settings

# models_grid$populations[[1]] -> pop
# save(pop, file = "testresults/pop.RData")
# models_grid$relations[[1]] -> rel
# save(rel, file = "testresults/rel.RData")

models_grid %>% write_all_models_to_files(dir_path = "../gluesless/test_data/model_grid")

#### test working with gluesless ####

models_grid$simulation_results <- run_gluesless(
  app_path = "/home/clemens/neomod/gluesless/build/gluesless",
  input_file_dir = "/home/clemens/neomod/gluesless/test_data/model_grid",
  output_file_dir = "/home/clemens/neomod/gluesless/test_data/model_grid",
  models_to_run = models_grid$model_id
)

####

models_grid %<>% calculate_all_idea_proportions_over_time()

library(ggplot2)

models_grid$idea_proportions[[4]] %>%
  ggplot() +
    geom_area(aes(x = timesteps, y = individuals_with_variant, fill = variant, group = variant)) +
    geom_line(aes(x = timesteps, y = individuals_with_variant, group = variant), position = "stack") +
    theme_bw() +
    xlab(expression(paste("t"))) +
    ylab("variants and their occurence in the population [%]")


idea_proportions <- dplyr::bind_rows(models_grid$idea_proportions, .id = 'source')

idea_proportions %>% 
  ggplot(aes(x = timesteps, y = individuals_with_variant, color = source)) +
    geom_line(alpha = 0.4) +
    theme_bw() +
    facet_wrap(~variant) +
    stat_smooth(method = "loess", formula = y ~ x, size = 1, span = 0.1) +
    xlab(expression(paste("t"))) 

# pop %>%
#   tibble::as.tibble() %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(
#     years = list(birth_time:death_time)
#   ) %>%
#   dplyr::ungroup()

#### analyse result ####

population_development_plot <- plot_population_development(pop)
relations_development_plot <- plot_relations_development(pop, rel)

library(cowplot)
cowplot::plot_grid(
  population_development_plot,
  relations_development_plot,
  align = "v",
  nrow = 2,
  labels = "AUTO"
)

# hu %>%
#   ggplot() +
#     geom_area(aes(x = time, y = n, fill = unit, group = unit)) +
#     geom_line(aes(x = time, y = n, group = unit), position = "stack") +
#     theme_bw()


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



#### setup settings grid ####

# create populations_grid data.frame
models_grid <- expand.grid(
  # general settings
  timeframe = list(
    0:1400
  ),
  # population settings  
  population_size_functions = c(
    function(t) {50},
    function(t) {round(0.0001 * (t - 700)^2 + 30, 0)}
    # function(t) {round(0.0005 * (t - 1000)^2 + 100, 0)}
    # function(t) {round(0.0005 * (t - 1000)^2 + 100, 0)}
    # function(t) {round(2000 - 0.95 * t, 0)}
  ),
  units_amount = c(
    5
  ),
  age_distribution_functions = c(
    function(x) {1 / (1 + 0.0004 * 0.7^(-7*log(x)))}
  ),
  age_ranges = list(
    1:100
  ),
  # relations settings
  amounts_friends = list(
    20
  ),
  unit_interaction_matrix = list(
    t(matrix(
      c(
        0,1,1,1,1,
        1,0,1,1,1,
        1,1,0,1,1,
        1,1,1,0,1,
        1,1,1,1,0
      ),
      5, 5
    ))
  ),
  cross_unit_proportion_child_of = list(
    0.01,
    0.1,
    0.5
  ),
  cross_unit_proportion_friend = list(
    0.01
  ),
  weight_child_of = list(
    3
  ),
  weight_friend = list(
    2
  ),
  # ideas settings
  names = list(
    c("idea_1", "idea_2")
  ),
  start_distribution = list(
    c(0.5, 0.5)
  ), 
  strength = list(
    c(1, 1) 
  )
) %>% tibble::as.tibble() %>%
  dplyr::mutate(
    multiplier = 1:nrow(.)
  ) %>%
  tidyr::uncount(10) %>%
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

models_grid %<>% calculate_all_idea_proportions_over_time(by_unit = FALSE)

library(ggplot2)

idea_proportions <- dplyr::bind_rows(models_grid$idea_proportions)

idea_proportions %>% 
  ggplot(aes(x = timesteps, y = individuals_with_variant, color = as.factor(multiplier), group = model_id)) +
    geom_line(alpha = 0.4) +
    theme_bw() +
    #facet_wrap(~variant) +
    facet_wrap(as.factor(multiplier)~variant) +
    stat_smooth(method = "loess", formula = y ~ x, size = 1, span = 0.2) +
    xlab(expression(paste("t"))) 

idea_proportions %>% 
  ggplot(aes(x = timesteps, y = individuals_with_variant, fill = variant, group = variant)) +
  geom_area() +
  geom_line(alpha = 0.4, position="stack",  color = "black") +
  theme_bw() +
  #facet_wrap(~variant) +
  facet_wrap(~model_id) +
  xlab(expression(paste("t"))) 

idea_proportions %>% 
  ggplot(aes(x = timesteps, y = individuals_with_variant, color = variant, group = variant)) +
  geom_line(alpha = 0.4) +
  theme_bw() +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, span = 0.2) +
  facet_wrap(~model_id) +
  xlab(expression(paste("t"))) 

idea_proportions %>% 
  tidyr::spread(variant, individuals_with_variant) %>%
  ggplot(aes(x = idea_1, y = idea_2, z = not_involved)) +
  geom_point(alpha = 0.4) +
  theme_bw() +
  #stat_smooth(method = "loess", formula = y ~ x, size = 1, span = 0.2) +
  facet_wrap(~model_id) +
  ggtern::coord_tern()

#### 

models_grid$idea_proportions[[9]] -> test_prop

test_prop %>%
  ggplot(aes(x = timesteps, y = individuals_with_variant, color = variant, group = variant)) +
  geom_line(alpha = 0.4) +
  theme_bw() +
  stat_smooth(method = "loess", formula = y ~ x, size = 1, span = 0.2) +
  xlab(expression(paste("t"))) 

moving_average <- function(x, n = 5, sides = 1) {
  as.vector(stats::filter(x, rep(1/n, n), sides = sides))
}

test_prop_smooth <- test_prop %>% 
  dplyr::group_by(variant) %>%
  dplyr::mutate(
    individuals_with_variant = moving_average(individuals_with_variant, n = 50, sides = 2)
  )

ggplot() +
  geom_line(
    data = test_prop,
    mapping = aes(x = timesteps, y = individuals_with_variant, color = variant, group = variant), 
    alpha = 0.4
  ) +
  geom_line(
    data = test_prop_smooth,
    mapping = aes(x = timesteps, y = individuals_with_variant, color = variant, group = variant), 
    alpha = 1
  ) +
  theme_bw() +
  xlab(expression(paste("t"))) 

####

huup <- idea_proportions %>%
  dplyr::filter(variant == "idea_1") %>%
  dplyr::group_by(model_id) %>%
  dplyr::mutate(
    individuals_with_variant = moving_average(individuals_with_variant, n = 50, sides = 2)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(timesteps, multiplier) %>%
  dplyr::summarise(
    min = min(individuals_with_variant),
    max = max(individuals_with_variant),
    mean = mean(individuals_with_variant),
    standard_deviation = sd(individuals_with_variant),
    range = abs(min - max),
    lower_quart = quantile(individuals_with_variant, na.rm = TRUE)[2],
    upper_quart = quantile(individuals_with_variant, na.rm = TRUE)[4],
    inter_quart_dist = abs(upper_quart - upper_quart)
  ) %>%
  dplyr::ungroup()

huup %>%
  ggplot() +
  geom_ribbon(aes(x = timesteps, ymin = min, ymax = max)) +
  geom_line(aes(x = timesteps, y = mean)) +
  geom_line(aes(x = timesteps, y = mean + standard_deviation), color = "red") +
  geom_line(aes(x = timesteps, y = mean - standard_deviation), color = "red") +
  geom_line(aes(x = timesteps, y = lower_quart), color = "darkgreen") +
  geom_line(aes(x = timesteps, y = upper_quart), color = "darkgreen") +
  facet_wrap(~multiplier)

huup %>%
  ggplot() +
  geom_line(aes(x = timesteps, y = range, color = as.factor(multiplier), group = as.factor(multiplier)))

huup %>%
  ggplot() +
  geom_line(aes(x = timesteps, y = inter_quart_dist, color = as.factor(multiplier), group = as.factor(multiplier)))

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



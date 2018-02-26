library(ggplot2)

population_size <- function(t) {
  round((cos(0.01 * t) + 3) * 100 + 0.2 * t, 0)
}

data.frame(t = 1:2000, n = population_size(1:2000)) %>%
  ggplot() +
  geom_line(aes(t, n))

population_age_distribution <- function(t) {
  function(n) {
    as.integer(abs(rnorm(n, 0, sd = 30)))
  }
}

population_sex_distribution <- function(t) {
  function(n) {
    sample(c("male", "female"), n, replace = TRUE)
  }
}

unit_number <- function(t) {
  round((sin(0.005 * t) + 3) * 2, 0)
}

data.frame(t = 1:2000, n = unit_number(1:2000)) %>%
  ggplot() +
  geom_line(aes(t, n))

population_unit_distribution <- function(t) {
  function(n, unit_vector) {
    sample(unit_vector, n, replace = TRUE)
  }
}

# innovation_openness <- function(age) {
#   # logistic function
#   1 / (1 + 0.001 * 0.7^(-7*log(age)))
# }
#
# plot(0:100, innovation_openness(0:100))

generate_humans <- function(current_time, start_id, number, start_age = NA, unit_vector) {
  # if (number > 0) {
    tibble::tibble(
      id = start_id:(start_id + number - 1),
      death_age = population_age_distribution(current_time)(number),
      current_age = if (is.na(start_age)) {
          as.integer(runif(number, min = 0, max = death_age))
        } else {
          as.integer(start_age)
      },
      dead = FALSE,
      birth_time = current_time - current_age,
      death_time = current_time + (death_age - current_age),
      sex = population_sex_distribution(current_time)(number),
      unit = population_unit_distribution(current_time)(number, unit_vector),
      unit_dead = FALSE
    ) %>% return()
  # } else {
  #   tibble::tibble(
  #     id = integer(),
  #     death_age = integer(),
  #     current_age = integer(),
  #     dead = logical(),
  #     birth_time = integer(),
  #     death_time =  integer(),
  #     sex = character(),
  #     unit = integer()
  #   )
  # }
}

start_units = 1:10
humans <- generate_humans(
  current_time = 0,
  start_id = 1,
  number = population_size(0),
  start_age = NA,
  unit_vector = start_units
  )
time <- 1:2000
unit_counter <- length(unique(humans$unit))

pb <- txtProgressBar(style = 3)
for (t in time) {
  setTxtProgressBar(pb, t/length(time))

  humans[!humans$dead, ] %<>%
    dplyr::mutate(
      current_age = as.integer(current_age + 1),
      dead = current_age >= death_age
    )

  alive <- nrow(humans[!humans$dead, ])
  # deaths <- sum(humans$death_time == t, na.rm = TRUE)

  if (alive >= population_size(t)) {
    next
  } else {
    births <- round(population_size(t) - alive, 0)
  }

  units <- unique(humans$unit[!humans$unit_dead])

  if (length(units) > unit_number(t)) {
    difference <- length(units) - unit_number(t)
    new_unit_vector <- sample(units, unit_number(t))
    humans[!humans$unit_dead, ] %<>%
      dplyr::mutate(
        unit_dead = ifelse(unit_dead %in% new_unit_vector, FALSE, TRUE)
      )
  } else {
    difference <- unit_number(t) - length(units)
    new_unit_vector <- c(units, (unit_counter + 1):(unit_counter + difference + 1))
    unit_counter <- unit_counter + difference
  }

  humans %<>% rbind(
    generate_humans(
      current_time = t,
      start_id = max(humans$id + 1),
      number = births,
      start_age = 0,
      unit_vector = new_unit_vector
    )
  )
}
close(pb)

humans %<>% dplyr::select(
  -current_age, -dead
)

# hu %>%
#  igraph::graph_from_data_frame(d = ., directed = FALSE)  %>%
#   visNetwork::visIgraph(layout = "layout_in_circle")


population_real <- tibble::tibble(t = time) %>%
  dplyr::mutate(
    n = t %>% purrr::map_int(
      function(x) {
        humans %>% dplyr::filter(
          birth_time <= x & x <= death_time
        ) %>%
          nrow() %>%
          return()
      }
    )
  )

population_expected <- tibble::tibble(
  t = time,
  n = population_size(t)
)

unit_size <- humans %>%
  dplyr::group_by(
    unit
  ) %>%
  dplyr::summarise(n = n())

library(ggplot2)
humans %>%
  ggplot() +
  geom_segment(
    aes(y = id, yend = id, x = birth_time, xend = death_time, color = unit)
  ) +
  #facet_wrap(~unit) +
  geom_vline(aes(xintercept = time[1])) +
  geom_vline(aes(xintercept = time[length(time)])) +
  geom_line(
    data = population_expected,
    aes(x = t, y = n - max(population_expected$n) - 50),
    color = "black"
  ) +
  geom_line(
    data = population_real,
    aes(x = t, y = n - max(population_expected$n) - 50),
    color = "red"
  )

#### relations ####

heavy_sexing <- function(x) {
  from <- c()
  to <- c()
  type <- c()
  start_time <- c()
  end_time <- c()

  pb <- txtProgressBar(style = 3)
  for (child in 1:nrow(x)) {
    potential_parents <- x %>% dplyr::filter(
      (birth_time + 12) <= x$birth_time[child],
      x$birth_time[child] <= (birth_time + 60),
      unit == x$unit[child]
    )
    if (!all(c("male", "female") %in% unique(potential_parents$sex))) {next}
    mother <- potential_parents %>%
      dplyr::filter(sex == "female") %>%
      dplyr::sample_n(1) %$%
      id
    father <- potential_parents %>%
      dplyr::filter(sex == "male") %>%
      dplyr::sample_n(1) %$%
      id
    from <- append(from, c(mother, mother, father))
    to <- append(to, c(father, child, child))
    type <- append(type, c("sexing", "child_of", "child_of"))
    #label <- append(label, c("❤", "✌", "✌"))
    start_time <- append(start_time, x$birth_time[child] - 1, x$birth_time[child], x$birth_time[child])
    end_time <- append(end_time, x$birth_time[child] + 2, x$birth_time[child] + 14, x$birth_time[child] + 14)
    setTxtProgressBar(pb, child/nrow(x))
  }
  close(pb)

  tibble::tibble(from, to, label)
}

humans %>% heavy_sexing() -> hu


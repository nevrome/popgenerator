test2$populations[[4]] -> pop
test2$relations[[4]] -> rel

pop_groups <- pop %>% 
  dplyr::group_by(unit) %>%
  dplyr::mutate(
    timeblock = plyr::round_any(birth_time, 100, f = floor)
  ) %>%
  dplyr::group_by(unit, timeblock) %>%
  dplyr::summarise(
    n = n()
  )

frommer <- dplyr::left_join(
  rel[, c("from", "type")],
  pop,
  by = c("from" = "id")
) %>%
  dplyr::select(
    from, birth_time, unit, type
  ) %>%
  dplyr::rename(
    "from_birth_time" = "birth_time",
    "from_unit" = "unit"
  )

toer <- dplyr::left_join(
  rel[, c("to")],
  pop,
  by = c("to" = "id")
) %>%
  dplyr::select(
    to, birth_time, unit
  ) %>%
  dplyr::rename(
    "to_birth_time" = "birth_time",
    "to_unit" = "unit"
  )

rel2 <- cbind(frommer, toer)

rel3 <- rel2 %>%
  dplyr::filter(
    from_unit != to_unit
  ) %>%
  dplyr::mutate(
    timeblock_from = plyr::round_any(from_birth_time, 100, f = floor),
    timeblock_to = plyr::round_any(to_birth_time, 100, f = floor)
  ) %>%
  dplyr::select(
    -from, -to, -from_birth_time, -to_birth_time
  )

rel4 <- rel3 %>% dplyr::group_by(
  from_unit, to_unit, timeblock_from, timeblock_to
) %>%
  dplyr::summarise(
    n = n(),
    type = type[1]
  )

# rel5 <- rel4 %>% dplyr::filter(
#   n > 6
# )

library(ggplot2)
ggplot() +
  geom_point(data = pop_groups, aes(x = unit, y = timeblock, size = n)) +
  geom_segment(
    data = rel4,#[rel4$type != "friend", ], 
    aes(
      x = from_unit, xend = to_unit, 
      y = timeblock_from, yend = timeblock_to,
      color = type
    ),
    size = 1,
    alpha = 0.05
  ) +
  scale_color_manual(
    values = c(
      "child_of" = "red",
      "friend" = "blue"
    )
  )


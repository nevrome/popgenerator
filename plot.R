library(ggplot2)
library(magrittr)

test <- readr::read_csv("test.csv")

test

test_plot <- test %>% ggplot() +
  geom_point(aes(x = time, y = social))

ggsave(
  "test_plot.png",
  test_plot,
  device = "png"
)

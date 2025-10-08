library(tidyverse)
mtcars |> as_tibble()


mtcars |> filter(hp < 100) |> group_by(vs) |> summarise(mean(carb))

mpg
mpg |>
    ggplot(aes(disp, hwy)) +
    geom_point() +
    geom_line(data = grid)

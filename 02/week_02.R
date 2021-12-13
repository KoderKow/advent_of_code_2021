library(tidyverse)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   Part 1                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d_init <- read_lines("02/data.txt")

d <-
  d_init |>
  enframe(name = "id") |>
  select(-id) |>
  separate(
    col = value,
    into = c("position", "unit"),
    convert = TRUE
  ) |>
  mutate(
    unit = if_else(
      condition = position == "up",
      true = unit * -1L,
      false = unit
    ),
    group = if_else(
      condition = position == "forward",
      true = "x",
      false = "y"
    )
  )|>
  pivot_wider(
    names_from = group,
    values_from = unit
  )

d |>
  summarize(
    x = sum(x),
    y = sum(y),
    solution = x * y
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   Part 2                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d |>
  mutate(
    aim = cumsum(ifelse(group == "y", unit, 0))
  ) |>
  filter(group == "x") |>
  summarize(
    x = sum(unit),
    y = sum(unit * aim),
    solution = x * y
  )

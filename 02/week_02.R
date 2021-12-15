library(tidyverse)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   Part 1                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d_init <- read_tsv(
  file = "02/data.txt",
  col_names = FALSE
  )

d <-
  d_init |>
  separate(
    col = X1,
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
  )

d |>
  group_by(group) |>
  summarize(
    units = sum(unit)
  ) |>
  pull(units) |>
  prod()

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

library(tidyverse)

d_init <-
  read_tsv(
    file = "03/input.txt",
    col_names = FALSE
  )

n_cols <- nchar(d_init$X1[1])

col_names <- paste0(
  "col_",
  str_pad(0:n_cols, width = 2, pad = 0)
)

d <-
  d_init |>
  separate(
    col = X1,
    into = col_names,
    sep = "",
    convert = TRUE
  ) |>
  select(-1)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   Part 1                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
d_1 <-
  d |>
  pivot_longer(
    cols = everything()
  ) |>
  count(name, value) |>
  group_by(name) |>
  summarize(
    gamma = value[which.max(n)],
    epsilon = value[which.min(n)]
  )

gamma <-
  paste0(d_1$gamma, collapse = "") |>
  strtoi(base = 2)

epsilon <-
  paste0(d_1$epsilon, collapse = "") |>
  strtoi(base = 2)

gamma * epsilon

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                   Part 2                                 ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Recreate the steps in the example
example_d_init <- c(
  "00100", "11110", "10110",
  "10111", "10101", "01111",
  "00111", "11100", "10000",
  "11001", "00010", "01010"
  ) |>
  enframe(value = "X1")

example_n_cols <- nchar(example_d_init$X1[1])

example_col_names <- paste0(
  "col_",
  str_pad(0:example_n_cols, width = 2, pad = 0)
)

example_d <-
  example_d_init |>
  separate(
    col = X1,
    into = example_col_names,
    sep = "",
    convert = TRUE
  ) |>
  select(-(1:2))

i <- 1
i_max <- ncol(example_d)
d_oxygen <- d_co2 <- example_d

while (i <= i_max) {
  ref_col <- rlang::sym(example_col_names[i + 1])

  d_oxygen_count <-
    d_oxygen |>
    add_count({{ ref_col }}, name = "n")

  d_co2_count <-
    d_co2 |>
    add_count({{ ref_col }}, name = "n")

  if (length(unique(d_oxygen_count$n)) == 1) {
    d_oxygen <-
      d_oxygen_count |>
      filter({{ ref_col }} == 1)

    d_co2 <-
      d_co2_count |>
      filter({{ ref_col }} == 0)

  } else {
    d_oxygen <-
      d_oxygen_count |>
      filter(n == max(n))

    d_co2 <-
      d_co2_count |>
      filter(n == min(n))
  }

  i <- i + 1
}

oxygen <-
  d_oxygen |>
  select(-n) |>
  unite(
    col = binary,
    everything(),
    sep = ""
  ) |>
  mutate(decimal = strtoi(binary, base = 2))

co2 <-
  d_co2 |>
  select(-n) |>
  unite(
    col = binary,
    everything(),
    sep = ""
  ) |>
  mutate(decimal = strtoi(binary, base = 2))

## Matches!
oxygen$decimal * co2$decimal

## Solution
i <- 1
i_max <- ncol(d)
d_oxygen <- d_co2 <- d

while (i <= i_max) {
  ref_col <- rlang::sym(col_names[i + 1])

  d_oxygen_count <-
    d_oxygen |>
    add_count({{ ref_col }}, name = "n")

  d_co2_count <-
    d_co2 |>
    add_count({{ ref_col }}, name = "n")

  if (nrow(d_oxygen_count) == 1) {
    ## If there is only one row, do nothing
    d_oxygen <- d_oxygen_count

  } else if (length(unique(d_oxygen_count$n)) == 1) {
    ## If all counts are the same, keep the 1's
    d_oxygen <-
      d_oxygen_count |>
      filter({{ ref_col }} == 1)

    ## If there are no 1's, do not remove anything
    if (nrow(d_oxygen) == 0) {
      d_oxygen <- d_oxygen_count
    }

  } else {
    ## Keep the most common value
    d_oxygen <-
      d_oxygen_count |>
      filter(n == max(n))
  }

  if (nrow(d_co2_count) == 1) {
    ## If there is only one row, do nothing
    d_co2 <- d_co2_count

  } else if (length(unique(d_co2_count$n)) == 1) {
    ## If all counts are the same, keep the 0's
    d_co2 <-
      d_co2_count |>
      filter({{ ref_col }} == 0)

    ## If there are no 0's, do not remove anything
    if (nrow(d_co2) == 0) {
      d_co2 <- d_co2_count
    }

  } else {
    ## Keep the least common value
    d_co2 <-
      d_co2_count |>
      filter(n == min(n))
  }

  i <- i + 1
}

oxygen <-
  d_oxygen |>
  select(-n) |>
  unite(
    col = binary,
    everything(),
    sep = ""
  ) |>
  mutate(decimal = strtoi(binary, base = 2))

co2 <-
  d_co2 |>
  select(-n) |>
  unite(
    col = binary,
    everything(),
    sep = ""
  ) |>
  mutate(decimal = strtoi(binary, base = 2))

oxygen$decimal * co2$decimal

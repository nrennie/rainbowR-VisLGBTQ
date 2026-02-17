library(tidyverse)
library(readxl)


# Scotland ----------------------------------------------------------------

sexual_age_scot <- read_xlsx("raw-data/Scotland/sexual_age_scot.xlsx", skip = 11) |>
  select(-1) |>
  rename(Sex = "...2", Age = "...3") |>
  fill(Sex, .direction = "down") |>
  filter(row_number() != 1) |>
  drop_na()
write_csv(sexual_age_scot, "data/sexual_age_scot.csv")

genmod_age_scot <- read_xlsx("raw-data/Scotland/genmod_age_scot.xlsx", skip = 11) |>
  select(-1) |>
  rename(Age = "...2") |>
  filter(row_number() != 1) |>
  drop_na()
write_csv(genmod_age_scot, "data/genmod_age_scot.csv")


# Northern Ireland --------------------------------------------------------

sexual_age_ni_raw1 <- read_xlsx("raw-data/Northern Ireland/sexual_age_ni.xlsx",
                           sheet = 2, skip = 8) |>
  filter(row_number() <= 12) |>
  pivot_longer(-c(Geography, `Geography code`),
               values_to = "Count") |>
  separate_wider_delim(
    cols = name,
    delim = ":", names = c("Age", "Orientation"),
    too_few = "align_start"
  ) |>
  mutate(Orientation = str_trim(Orientation),
         Orientation = if_else(is.na(Orientation), "All", Orientation)) |>
  mutate(Age = str_replace(Age, "All usual residents", "Usual residents"))

sexual_age_ni_raw2 <- read_xlsx("raw-data/Northern Ireland/sexual_age_ni.xlsx",
                               sheet = 2, skip = 23) |>
  drop_na() |>
  pivot_longer(-c(Geography, `Geography code`),
               values_to = "Percentage") |>
  separate_wider_delim(
    cols = name,
    delim = ":", names = c("Age", "Orientation"),
    too_few = "align_start"
  ) |>
  mutate(Orientation = str_trim(Orientation),
         Orientation = if_else(is.na(Orientation), "All", Orientation)) |>
  mutate(Age = str_replace(Age, "All usual residents", "Usual residents")) |>
  filter(Percentage <= 1)

sexual_age_ni <- sexual_age_ni_raw1 |>
  left_join(sexual_age_ni_raw2, by = c("Geography", "Geography code", "Age", "Orientation")) |>
  mutate(Percentage = 100 * Percentage)
write_csv(sexual_age_ni, "data/sexual_age_ni.csv")



# England & Wales ---------------------------------------------------------

sexual_genmod_ew <- read_csv("raw-data/England and Wales/sexual_genmod.csv")



sexual_age_sex <- read_csv("raw-data/England and Wales/Sexual orientation/sexual_age_sex.csv")

sexual_disability <- read_csv("raw-data/England and Wales/Sexual orientation/sexual_disability.csv")

sexual_general_health <- read_csv("raw-data/England and Wales/Sexual orientation/sexual_general_health.csv")

sexual_age_sex |>
  View()


# maps
# zoom in bar chart
# beeswarm (pop / percentage)





















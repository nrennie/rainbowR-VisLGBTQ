library(tidyverse)
library(readxl)


# Scotland ----------------------------------------------------------------

sexual_age_scot <- read_xlsx("raw-data/Scotland/sexual_age_scot.xlsx", skip = 11) |>
  select(-1) |>
  rename(Sex = "...2", Age = "...3") |>
  fill(Sex, .direction = "down") |>
  filter(row_number() != 1) |>
  drop_na() |>
  pivot_longer(-c(Sex, Age), names_to = "response", values_to = "n") |>
  rename(sex = Sex, age = Age) |>
  mutate(
    response = str_trim(response),
    response = if_else(response == "All people aged 16 and over", "Total", response)
  ) |>
  mutate(sex = if_else(sex == "All people aged 16 and over", "All", sex))
write_csv(sexual_age_scot, "data/sexual_age_scot.csv")

genmod_age_scot <- read_xlsx("raw-data/Scotland/genmod_age_scot.xlsx", skip = 11) |>
  select(-1) |>
  rename(Age = "...2") |>
  filter(row_number() != 1) |>
  drop_na() |>
  pivot_longer(-Age, names_to = "response", values_to = "n") |>
  rename(age = Age) |>
  mutate(
    response = str_trim(response),
    response = if_else(response == "All people aged 16 and over", "Total", response)
  )
write_csv(genmod_age_scot, "data/genmod_age_scot.csv")


# Northern Ireland --------------------------------------------------------

sexual_age_ni_raw1 <- read_xlsx("raw-data/Northern Ireland/sexual_age_ni.xlsx",
  sheet = 2, skip = 8
) |>
  filter(row_number() <= 12) |>
  pivot_longer(-c(Geography, `Geography code`),
    values_to = "count"
  ) |>
  separate_wider_delim(
    cols = name,
    delim = ":", names = c("age", "sexual_orientation"),
    too_few = "align_start"
  ) |>
  mutate(
    sexual_orientation = str_trim(sexual_orientation),
    sexual_orientation = if_else(is.na(sexual_orientation), "All", sexual_orientation)
  ) |>
  mutate(age = str_replace(age, "All usual residents", "Usual residents"))

sexual_age_ni_raw2 <- read_xlsx("raw-data/Northern Ireland/sexual_age_ni.xlsx",
  sheet = 2, skip = 23
) |>
  drop_na() |>
  pivot_longer(-c(Geography, `Geography code`),
    values_to = "percentage"
  ) |>
  separate_wider_delim(
    cols = name,
    delim = ":", names = c("age", "sexual_orientation"),
    too_few = "align_start"
  ) |>
  mutate(
    sexual_orientation = str_trim(sexual_orientation),
    sexual_orientation = if_else(is.na(sexual_orientation), "All", sexual_orientation)
  ) |>
  mutate(age = str_replace(age, "All usual residents", "Usual residents")) |>
  filter(percentage <= 1)

sexual_age_ni <- sexual_age_ni_raw1 |>
  left_join(sexual_age_ni_raw2, by = c("Geography", "Geography code", "age", "sexual_orientation")) |>
  mutate(percentage = 100 * percentage) |>
  rename(
    area_code = `Geography code`,
    area_name = Geography,
  )
write_csv(sexual_age_ni, "data/sexual_age_ni.csv")


# England & Wales ---------------------------------------------------------

sexual_genmod_ew <- read_csv("raw-data/England and Wales/sexual_genmod.csv") |>
  select(
    area_code = `Lower tier local authorities Code`,
    area_name = `Lower tier local authorities`,
    gender_identity = `Gender identity (7 categories)`,
    sexual_orientation = `Sexual orientation (4 categories)`,
    n = Observation
  )
write_csv(sexual_genmod_ew, "data/sexual_genmod_ew.csv")

# Population data
population_ew <- read_xlsx("raw-data/England and Wales/lsoa_population.xlsx", skip = 2) |>
  select(
    area_code = `LA code`,
    area_name = `LA name`,
    population = `Usual resident population, 2021`
  )
write_csv(population_ew, "data/population_ew.csv")

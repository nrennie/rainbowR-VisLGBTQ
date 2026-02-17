library(tidyverse)
library(ggview)
library(ggbeeswarm)

plot_data <- sexual_general_health |>
  select(`Sexual orientation (6 categories)`,
`General health (6 categories)`, Observation) |>
  rename(Orientation = `Sexual orientation (6 categories)`,
         Health = `General health (6 categories)`)


# Initial heatmap ---------------------------------------------------------

heatmap_data <- plot_data |>
  group_by(Orientation, Health) |>
  summarise(n = sum(Observation))

ggplot() +
  geom_tile(
    data = heatmap_data,
    mapping = aes(x = Orientation, y = Health, fill = n)
  )


# Percentage within categories --------------------------------------------

heatmap_data <- plot_data |>
  group_by(Orientation) |>
  mutate(n_orient = sum(Observation)) |>
  group_by(Orientation, Health) |>
  mutate(n_obs = sum(Observation)) |>
  select(-Observation) |>
  distinct() |>
  mutate(avg_perc = n_obs / n_orient) |>
  drop_na()


ggplot() +
  geom_tile(
    data = heatmap_data,
    mapping = aes(x = Orientation, y = Health, fill = avg_perc)
  )

# Sort health levels
heatmap_data$Health <- factor(heatmap_data$Health, levels = rev(c(
  "Very good health", "Good health", "Fair health", "Bad health", "Very bad health", "Does not apply"
)))

ggplot() +
  geom_tile(
    data = heatmap_data,
    mapping = aes(x = Orientation, y = Health, fill = avg_perc)
  )

# colour











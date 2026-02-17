library(tidyverse)
library(sf)

sexual_age_ni |>
  filter(area_name != "Northern Ireland",
         age == "Usual residents aged 16 and over")


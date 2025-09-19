#' ----------------------------
#' This is a quick check of which species occur at
#' what sites and at what depth for use by the 
#' thermal occupancy mapping
#' ----------------------------

library(dplyr)
library(readr)

# go with raw data so unfiltered


substrate <- 
  read_csv("data/sebens_substrate_proportion_20250908.csv") |>
  filter(year > 1970,
         month %in% c(6:9),
         angle == "vertical") |>
  group_by(site, year) |>
  slice_tail() |> # get second sample point if there are 2
  ungroup()

names(substrate)


substrate_long <- substrate |>
  
  # pivot longer so attributes (i.e., species) are in rows  
  tidyr::pivot_longer(c(alcyonium_sub : tubularia_sub),
               names_to = "species",
               values_to = "proportion")

substrate_obs_count <- substrate_long |>
  group_by(species, area, depth_m) |>
  summarize(num_obs = length(proportion)) |>
  ungroup()

write_csv(substrate_obs_count, "data/sp_presence_over_time.csv")

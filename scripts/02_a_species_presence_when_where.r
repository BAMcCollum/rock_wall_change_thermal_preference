#' ----------------------------
#' This is a quick check of which species occur at
#' what sites and at what depth for use by the 
#' thermal occupancy mapping
#' ----------------------------

library(dplyr)
library(readr)
source("scripts/load_data_and_provide_constants.R")

names(substrate)

substrate_obs_count <- substrate_long |>
  group_by(species, area, depth_m) |>
  summarize(num_obs = length(proportion)) |>
  ungroup()

write_csv(substrate_obs_count, "data/sp_presence_over_time.csv")

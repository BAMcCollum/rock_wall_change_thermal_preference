#' -----------------------------------------------
#' This script merges coefficients and the species
#' thermal info generated from
#' https://github.com/BAMcCollum/sp_thermal_limits
#' -----------------------------------------------

pacman::p_load(dplyr, readr)

dict <- read_csv("data/co_occuring_species.csv")
sp_coefs <- read_csv("data/change_coefficients.csv")
indices <- read_csv("data/Occurrence_based_species_thermal_indicies_Photos_20250103.csv")


# First, translate indices species into coef species names
indices <- indices |>
  rename(co_occuring_species = gen_spp) |>
  left_join(dict) |>
  rename(species = coefficients_species) |>
  filter(!is.na(species))

# Then, join the indices to the coefs

combined_data <- left_join(sp_coefs, indices) |>
  janitor::remove_empty(which = "cols")

write_csv(combined_data, "data/coefs_with_indices.csv")

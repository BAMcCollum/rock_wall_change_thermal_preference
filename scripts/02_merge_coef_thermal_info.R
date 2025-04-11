#' -----------------------------------------------
#' This script merges coefficients and the species
#' thermal info generated from
#' https://github.com/BAMcCollum/sp_thermal_limits
#' -----------------------------------------------

pacman::p_load(dplyr, readr)

dict <- read_csv("data/co_occuring_species.csv")
sp_coefs <- read_csv("data/change_coefficients_ordbetareg.csv")
indices <- read_csv("data/Occurrence_based_species_thermal_indicies_Photos_20250103.csv")


# First, translate indices species into coef species names
indices_joined <- indices |>
  left_join(dict) |>
  rename(species = coefficients_species) |>
  filter(!is.na(species)) |>
  relocate(species)

# Then, join the indices to the coefs

combined_data <- left_join(sp_coefs, indices_joined) |>
  janitor::remove_empty(which = "cols")

View(combined_data)

write_csv(combined_data, "data/coefs_with_indices.csv")

combined_data <- read_csv("data/coefs_with_indices.csv")

combined_data <- na.omit(combined_data) 

combined_data %>%
  arrange(estimate) %>%
  mutate(gen_spp=factor(gen_spp, levels=gen_spp)) %>%
  ggplot() +
  geom_pointrange(mapping = aes(x=gen_spp, y=estimate, ymin = conf.low, ymax = conf.high)) +
  geom_hline(yintercept = 0, lty = 2, color = "grey")+
  coord_flip() +
  theme_bw(base_size = 18)+
  ylab("Coefficient of Change")+
  xlab("Species")

  
ggsave(glue::glue("figures/coefs_of_change.pdf"))


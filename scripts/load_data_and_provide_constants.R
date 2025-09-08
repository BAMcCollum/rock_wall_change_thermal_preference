#' -----------------------------------------------
#' Helper script to load data and provide consistent
#' themes for plots
#' -----------------------------------------------

library(dplyr)
library(tidyr)
library(readr)



substrate <- 
  read_csv("data/sebens_substrate_proportion_20250908.csv") |>
  filter(year > 1970,
         month %in% c(6:9),
         angle == "vertical") |>
  group_by(site, year) |>
  slice_tail() |> # get second sample point if there are 2
  ungroup() |>
  mutate(#hydroid = hydroid_sub + tubularia_sub,
         #isodictya_deichmannae = isodictya_spp + orange_sponge_crust,
         year_cent = year - mean(year))

         

drop_cols <- c(#'hydroid_sub',
               #'tubularia_sub',
               #'isodictya_spp',
               #'orange_sponge_crust',
               'hymedesmia_sp', 
               'boltenia_ovifera', 
               'haliclona_oculata'
              )


substrate <- substrate |>
  select(-one_of(drop_cols))


substrate_long <- substrate |>
  
  # pivot longer so attributes (i.e., species) are in rows  
  pivot_longer(c(alcyonium_sub : tube_complex),
               names_to = "species",
               values_to = "proportion")


## 
# remove rare species
# where rare = 1 year or less of > 5% cover
##
rare_sp <- substrate_long |>
  filter(!is.na(proportion)) |>
  group_by(species) |>
  summarize(n_year_abund = sum(proportion > 0.05)) |>
  filter(n_year_abund <= 1) |>
  pull(species)

substrate_long <- substrate_long |>
  filter(!(species %in% rare_sp))


##
# set visual themes
##

library(ggplot2)

theme_set(theme_classic(base_size = 18) +
          theme(axis.title.y = ggtext::element_markdown())
)


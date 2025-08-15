#' -----------------------------------------------
#' Helper script to load data and provide consistent
#' themes for plots
#' -----------------------------------------------

library(dplyr)
library(tidyr)
library(readr)



substrate <- 
  read_csv("data/sebens_substrate_proportion.csv") |>
  filter(year > 1970,
         month %in% c(6:9),
         angle == "vertical") |>
  group_by(site, year) |>
  slice_tail() |> # get second sample point if there are 2
  ungroup() |>
  mutate(hydroids = hydroid_sub + tubularia_sub,
         isodictya_spp = isodictya_spp + orange_sponge_crust,
         alcyonium_siderium = alcyonium_sub,
         anomia_simplex = anomia_spp,
         botrylloides_violaceus = botrylloides_sp,
         cliona_celata = cliona_spp,
         clathromorphum_circumscriptum = clathromorphum_sp,
         erect_bryozoan = erect_bryozoan_sub,
         green_algae = green_algae_sub,
         halisarca_spp = halisarca_nahantensis,
         leucosolenia_botryoides = leucosolenia_spp,
         lithothamnion_glaciale  = lithothamnion_spp,
         metridium_senile = metridium_sub,
         peysonnelia_rugulosum = peysonnelia,
         year_cent = year - mean(year))
         

drop_cols <- c('hymedesmia_sp', 
               'boltenia_ovifera', 
               'haliclona_oculata',
               'hydroid_sub',
               'tubularia_sub',
               'orange_sponge_crust',
               'alcyonium_sub',
               'anomia_spp',
               'botrylloides_sp',
               'cliona_spp',
               'clathromorphum_sp',
               'leucosolenia_spp',
               'erect_bryozoan_sub',
               'lithothamnion_spp',
               'peysonnelia',
               'metridium_sub')


substrate <- substrate |>
  select(-one_of(drop_cols))


substrate_long <- substrate |>
  
  # pivot longer so attributes (i.e., species) are in rows  
  pivot_longer(c(aplidium_glabrum : peysonnelia_rugulosum),
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

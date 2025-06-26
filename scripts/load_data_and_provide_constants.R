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
  mutate(year_cent = year - mean(year)) 

drop_cols <- c('hymedesmia_sp', 'boltenia_ovifera', 'haliclona_oculata')

substrate <- substrate |>
  select(-one_of(drop_cols))

substrate_long <- substrate |>
  
  # pivot longer so attributes (i.e., species) are in rows  
  pivot_longer(alcyonium_sub : tubularia_sub,
               names_to = "species",
               values_to = "proportion")


##
# set visual themes
##

library(ggplot2)

theme_set(theme_classic(base_size = 18) +
          theme(axis.title.y = ggtext::element_markdown())
)

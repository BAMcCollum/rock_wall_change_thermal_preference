#' -----------------------------------------------
#' This script calculates the change coefficient
#' for each species under an ordbeta regression
#' and outputs a file with species names and 
#' coefficients (and SEs)
#' -----------------------------------------------

library(readr)
library(glmmTMB)
library(visreg)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(broom.mixed)
library(broom)

substrate <- 
  read_csv("data/sebens_substrate_proportion.csv") |>
  filter(year > 1970,
         month %in% c(6:9),
         angle == "vertical") |>
  mutate(year_cent = year - mean(year))

View(substrate)

substrate_long <- substrate |>
  
  # pivot longer so attributes (i.e., species) are in rows  
  pivot_longer(alcyonium_sub : tubularia_sub,
               names_to = "species",
               values_to = "proportion") |>
  
  # fit a model to each attribute
  group_by(species) |>
  nest() |>
  mutate(mod = map(data, ~glmmTMB(proportion ~
                                    year_cent  + area + (1|site %in% area),
                                  family = ordbeta(link = "logit"), data = .x)), #.x is a placeholder for each nested df
         coef = map(mod, ~tidy(.x) |> filter(term == "year_cent"))) |>
  
  # great, get back the data
  unnest(coef) |>
  select(-data, -mod, -term) |>
  print (n = 63)

View(substrate_long)

write_csv(substrate_long,"data/change_coefficients.csv")

#The species behaving badly:

#boltenia_ovifera
#botryllus_schlosseri
#cliona_spp
#halocynthia_pyriformis
#hymedesmia_sp
#molgula_citrina
#mytilus_edulis
#orange_sponge_crust
#scypha_sp
#styela_sp

#' -----------------------------------------------
#' This script calculates the change coefficient
#' for each species under an ordbeta regression
#' and outputs a file with species names and 
#' coefficients (and SEs) using ordbetareg
#' -----------------------------------------------

library(readr)
library(ordbetareg)
library(visreg)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(broom.mixed)
library(broom)
options()

# load the data
source("scripts/load_data_and_provide_constants.R")

coefs_long <- substrate_long |>
 # debug, try with only a few species
 # filter(species %in% c("scypha_sp", "botryllus_schlosseri")) |>
  
  # fit a model to each attribute
  group_by(species) |>
  mutate(sp1 = species) |>
  nest() |>
  mutate(mod = map(data, ~ordbetareg(proportion ~
                                       year_cent  + area + (1|site %in% area),
                                      data = .x,
                                     file = glue::glue("abundance_models/{.x$sp1[1]}.rds"),
                                     file_refit = "on_change")), #.x is a placeholder for each nested df
         coef = map(mod, ~tidy(.x) |> filter(term == "year_cent"))) |>
  
  # great, get back the data
  unnest(coef) |>
  select(-data, -mod, 
         -effect, -component, -group) 

View(coefs_long)

write_csv(coefs_long,"data/change_coefficients_ordbetareg.csv")

#The species behaving badly with glmmTMB - so, let's try ordbetareg:

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

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
library(emmeans)


# load the data
source("scripts/load_data_and_provide_constants.R")

# helper function
get_fitted_values <- function(mod, adf){
  emmeans(mod, 
          specs = ~year_cent,
          epred = TRUE,
          at = list(year_cent = 
                      modelr::seq_range(adf$year_cent, 
                                        200))) |>
    tidy() |>
    mutate(year = year_cent + mean(adf$year))
}

mods_long <- substrate_long |>
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
         coef = map(mod, ~tidy(.x) |> filter(term == "year_cent")),
         fitted = map2(mod, data, get_fitted_values))

coefs_long <- mods_long |>  
  # great, get back the data
  unnest(coef) |>
  select(-data, -mod, 
         -effect, -component, -group, -fitted) |>
  arrange(species)

View(coefs_long)

write_csv(coefs_long,"data/change_coefficients_ordbetareg.csv")

# What about modeled predictions for figures marginalized
# over site and area?
fitted_long <- mods_long |>  
  arrange(species) |> 
  # great, get back the data
  unnest(fitted) |>
  select(-data, -mod, -coef) 

write_csv(fitted_long,"data/fitted_long_ordbetareg.csv")

#' -----------------------------------------------
#' This script creates a table of coefficients
#' and summary information about each timeseries model
#' -----------------------------------------------

library(brms)
library(broom.mixed)
library(dplyr)

# get models and matching species names
species <- list.files("abundance_models/",
                      pattern = "\\.rds") |> #use patern to only have models
  gsub("\\.rds", "", x=_)

models <- list.files("abundance_models",
                     pattern = "\\.rds",
                     full.names = TRUE) 

# function to get coefficient and r2 from each model
get_mod_info <- function(mod){
  mod <- readRDS(mod)
  year_coef <- suppressWarnings(tidy(mod, parameters = "year_cent"))
  fit <-  rstantools::bayes_R2(mod, re.form = NA) |> 
    as_tibble() 
  
  year_coef |>
    mutate(r2 = fit$Estimate) |>
    select(estimate, std.error, conf.low, conf.high, r2)
}

# iterate over models
mod_info <- purrr::map2(species, models, 
            ~get_mod_info(.y) |>
              mutate(species = .x))

# write it all out
mod_info |>
  purrr::list_rbind() |>
  relocate(species) |>
  readr::write_csv("tables/timeseries_coefs_fits.csv")

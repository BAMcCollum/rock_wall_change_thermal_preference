#' -----------------------------------------------
#' This script creates visualizations of timeseries
#' of species
#' -----------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(purrr)
library(glue)

# load data and set themes
source("scripts/load_data_and_provide_constants.R")
fitted_curves <- read_csv("data/fitted_long_ordbetareg.csv")

# add corrected species names
dict <- read_csv("data/co_occuring_species_20250919.csv") |>
  rename(species = coefficients_species)

fitted_curves <- left_join(fitted_curves, dict)
substrate_long <- left_join(substrate_long, dict)
  
# plot all species
species_data_as_list <- 
  substrate_long |>
  group_by(species) |>
  nest() |>
  mutate(data = map(data, ~.x |> mutate(species = species))) |>
  # pull out the data as a list
  pull(data) |>
  # walk through and make plots
  walk(~{
    # what species?
    print(.x$species[1])
    
    sp <- gsub("_", " ", .x$species[1])
    sp <- stringr::str_to_sentence(sp)
    sp <- gsub(" sub$", "", sp)
    y_lab <- glue("Percent Cover of {sp}")
    
    # correct some species names
    if(!is.na(.x$species_name[1])) {
      sp <- .x$species_name[1]
      y_lab <- glue("Percent Cover of *{sp}*")
    }
    
    
    ggplot(data = .x,
           aes(x = year, y = proportion*100, #sub in species name
               color = site)) +
      geom_line() +
      facet_wrap(~area) +
      labs(x = "Year",
           color = "Site",
           y = y_lab)+ #sub in species name
      theme(axis.title.y = ggtext::element_markdown())
    
    ggsave(glue("figures/raw_timeseries/{sp}.jpg"),
           width = 10, height = 6)
  })


  
# show representative species

str(substrate)

new_dom_sp <- substrate_long |>
  filter(species %in% c("erect_bryozoan_sub", 
                        "didemnum_vexillum",
                        "mytilus_edulis")) |>
  # change species names using mutate and case_when()
  mutate(species_name = 
           case_when(
             species == "erect_bryozoan_sub" ~ "Erect Byrozoan", 
             species == "didemnum_vexillum" ~ "Didemnum vexillum", 
             species == "mytilus_edulis" ~ "Mytilus edulis",
           )) 

new_dom_curves <- fitted_curves |>
  filter(species %in% unique(new_dom_sp$species))|>
  # change species names using mutate and case_when()
  mutate(species_name = 
           case_when(
             species == "erect_bryozoan_sub" ~ "Erect Byrozoan", 
             species == "didemnum_vexillum" ~ "Didemnum vexillum", 
             species == "mytilus_edulis" ~ "Mytilus edulis",
           )) 

# plot
ggplot(new_dom_sp,
       aes(x = year, y = proportion*100,
             group = site)) +
  geom_line(color = "grey") +
  geom_line(data = new_dom_curves,
            aes(y = estimate*100),
            color = "black", group = 1,
            linewidth=2) +
  geom_ribbon(data = new_dom_curves,
              aes(y = estimate*100,
                  ymin = lower.HPD*100,
                  ymax = upper.HPD*100),
              group = 1,
              alpha = 0.3) +
  labs(x = "Year",
       y = "Percent Cover") +
  facet_wrap(vars(species_name)) +
  theme(strip.text = element_text(face = "italic"))

ggsave(glue("figures/newly_dominant_species.jpg"),
       width = 9, height = 6)


dom_sp <- substrate_long |>
  filter(species %in% c("alcyonium_sub", 
                        "aplidium_glabrum",
                        "metridium_sub")) |>
  # change species names using mutate and case_when()
  mutate(species_name = 
           case_when(
             species == "alcyonium_sub" ~ "Alcyonium siderium", 
             species == "aplidium_glabrum" ~ "Aplidium glabrum", 
             species == "metridium_sub" ~ "Metridium senile"
           ))
  
dom_curves <- fitted_curves |>
  filter(species %in% unique(dom_sp$species))|>
  # change species names using mutate and case_when()
  mutate(species_name = 
           case_when(
             species == "alcyonium_sub" ~ "Alcyonium siderium", 
             species == "aplidium_glabrum" ~ "Aplidium glabrum", 
             species == "metridium_sub" ~ "Metridium senile"
           ))
# plot
ggplot(dom_sp,
         aes(x = year, y = proportion*100,
             group = site)) +
  geom_line(color = "grey") +
  geom_line(data = dom_curves,
            aes(y = estimate*100),
            color = "black", group = 1,
            linewidth=2) +
  geom_ribbon(data = dom_curves,
              aes(y = estimate*100,
                  ymin = lower.HPD*100,
                  ymax = upper.HPD*100),
              group = 1,
              alpha = 0.3) +
  labs(x = "Year",
       y = "Percent Cover") +
  facet_wrap(vars(species_name)) +
  theme(strip.text = element_text(face = "italic"))

ggsave(glue("figures/three_climax_species.jpg"),
       width = 9, height = 6)


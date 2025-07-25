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
    
    
    ggplot(data = .x,
           aes(x = year, y = proportion*100, #sub in species name
               color = site)) +
      geom_line() +
      facet_wrap(~area) +
      labs(x = "Year",
           color = "Site",
           y = glue("Percent Cover of *{sp}*"))+ #sub in species name
      theme(axis.title.y = ggtext::element_markdown())
    
    ggsave(glue("figures/raw_timeseries/{sp}.pdf"),
           width = 10, height = 6)
  })


  
# show representative species

str(substrate)

new_dom_sp <- substrate_long |>
  filter(species %in% c("erect_bryozoan_sub", 
                        "didemnum_vexillum",
                        "mytilus_edulis",
                        "lithothamnion_spp")) |>
  # change species names using mutate and case_when()
  mutate(species_name = 
           case_when(
             species == "erect_bryozoan_sub" ~ "Aborescent byrozoan", 
             species == "didemnum_vexillum" ~ "Didemnum vexillum", 
             species == "mytilus_edulis" ~ "Mytilus edulis",
             species == "lithothamnion_spp" ~ "Lithothamnion glaciale",
           )) 

new_dom_curves <- fitted_curves |>
  filter(species %in% unique(new_dom_sp$species))|>
  # change species names using mutate and case_when()
  mutate(species_name = 
           case_when(
             species == "erect_bryozoan_sub" ~ "Aborescent byrozoan", 
             species == "didemnum_vexillum" ~ "Didemnum vexillum", 
             species == "mytilus_edulis" ~ "Mytilus edulis",
             species == "lithothamnion_spp" ~ "Lithothamnion glaciale",
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
  facet_wrap(vars(species_name))

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
  facet_wrap(vars(species_name))

ggsave(glue("figures/three_climax_species.pdf"),
       width = 9, height = 6)


edwardsiella <- substrate_long |>
  filter(species == "edwardsiella_lineata") |>
  # change species names using mutate and case_when()
  mutate(species_name = 
           case_when(
             species == "edwardsiella_lineata" ~ "Edwardsiella lineata", 
             ))

edwardsiella_curves <- fitted_curves |>
  filter(species %in% unique(edwardsiella$species))|>
  # change species names using mutate and case_when()
  mutate(species_name = 
           case_when(
             species == "edwardsiella_lineata" ~ "Edwardsiella lineata", 
           ))
# plot
ggplot(edwardsiella,
       aes(x = year, y = proportion*100,
           group = site)) +
  geom_line(color = "grey") +
  geom_line(data = edwardsiella_curves,
            aes(y = estimate*100),
            color = "black", group = 1,
            linewidth=2) +
  geom_ribbon(data = edwardsiella_curves,
              aes(y = estimate*100,
                  ymin = lower.HPD*100,
                  ymax = upper.HPD*100),
              group = 1,
              alpha = 0.3) +
  labs(x = "Year",
       y = "Percent Cover") +
  facet_wrap(vars(species_name))










# 
# 
# #Use corrected names from coefs_with_indicies.csv
# # Plot alcyonium
# ggplot(data = substrate,
#        aes(x = year, y = alcyonium_sub, #sub in species name
#            color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Alcyonium siderium*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())
# 
# ggsave("figures/Alcyonium_siderium_plot.pdf")
# 
# 
# Anomia_simplex_plot <- ggplot(data = substrate,
#                               aes(x = year, y = anomia_spp, #sub in species name
#                                   color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Anomia simplex*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Anomia_simplex_plot.pdf")
# 
# Anomia_simplex_plot
# 
# Aplidium_glabrum_plot <- ggplot(data = substrate,
#                                 aes(x = year, y = aplidium_glabrum, #sub in species name
#                                     color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Aplidium glabrum*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Aplidium_glabrum_plot.pdf")
# 
# Aplidium_glabrum_plot
# 
# Balanus_balanus_plot <- ggplot(data = substrate,
#                                aes(x = year, y = balanus_balanus, #sub in species name
#                                    color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Balanus balanus*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Balanus_balanus_plot.pdf")
# 
# Balanus_balanus_plot
# 
# Boltenia_echinata_plot <- ggplot(data = substrate,
#                                  aes(x = year, y = boltenia_echinata, #sub in species name
#                                      color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Boltenia echinata*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Boltenia_echinata_plot.pdf")
# 
# Boltenia_echinata_plot
# 
# Boltenia_ovifera_plot <- ggplot(data = substrate,
#                                 aes(x = year, y = boltenia_ovifera, #sub in species name
#                                     color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Boltenia ovifera*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Boltenia_ovifera_plot.pdf")
# 
# Boltenia_ovifera_plot
# 
# Botrylloides_violaceus_plot <- ggplot(data = substrate,
#                                       aes(x = year, y = botrylloides_sp, #sub in species name
#                                           color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Botrylloides violaceus*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Botrylloides_violaceus_plot.pdf")
# 
# Botrylloides_violaceus_plot
# 
# Botryllus_schlosseri_plot <- ggplot(data = substrate,
#                                     aes(x = year, y = botryllus_schlosseri, #sub in species name
#                                         color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Botryllus schlosseri*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Botryllus_schlosseri_plot.pdf")
# 
# Botryllus_schlosseri_plot
# 
# Clathromorphum_circumscriptum_plot <- ggplot(data = substrate,
#                                         aes(x = year, y = clathromorphum_sp, #sub in species name
#                                             color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Clathromorphum circumscriptum*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Clathromorphum_circumscriptum_plot.pdf")
# 
# Clathromorphum_circumscriptum_plot
# 
# Cliona_celata_plot <- ggplot(data = substrate,
#                              aes(x = year, y = cliona_spp, #sub in species name
#                                  color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Cliona celata*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Cliona_celata_plot.pdf")
# 
# Cliona_celata_plot
# 
# Dendrodoa_carnea_plot <- ggplot(data = substrate,
#                                 aes(x = year, y = dendrodoa_carnea, #sub in species name
#                                     color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Dendrodoa carnea*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Dendrodoa_carnea_plot.pdf")
# 
# Dendrodoa_carnea_plot
# 
# Didemnum_albidum_plot <- ggplot(data = substrate,
#                                 aes(x = year, y = didemnum_albidum, #sub in species name
#                                     color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Didemnum albidum*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Didemnum_albidum_plot.pdf")
# 
# Didemnum_albidum_plot
# 
# Didemnum_vexillum_plot <- ggplot(data = substrate,
#                                  aes(x = year, y = didemnum_vexillum, #sub in species name
#                                      color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Didemnum vexillum*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Didemnum_vexillum_plot.pdf")
# 
# Didemnum_vexillum_plot
# 
# Dilosoma_listerianum_plot <- ggplot(data = substrate,
#                                     aes(x = year, y = diplosoma_listerianum, #sub in species name
#                                         color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Diplosoma listerianum*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Dilosoma_listerianum_plot.pdf")
# 
# Dilosoma_listerianum_plot
# 
# Edwardsiella_lineata_plot <- ggplot(data = substrate,
#                                     aes(x = year, y = edwardsiella_lineata, #sub in species name
#                                         color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Edwardsiella lineata*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Edwardsiella_lineata_plot.pdf")
# 
# Edwardsiella_lineata_plot
#
# Erect_bryozoan_sub_plot <- ggplot(data = substrate,
#                                     aes(x = year, y = erect_bryozoan_sub, #sub in species name
#                                         color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Arborescent Bryozoan*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())
# 
#   ggsave("figures/Edwardsiella_lineata_plot.pdf")
# 
# Erect_bryozoan_sub_plot
#
# Halichondria_panicea_plot <- ggplot(data = substrate,
#                                     aes(x = year, y = halichondria_panicea, #sub in species name
#                                         color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Halichondria panicea*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Halichondria_panicea_plot.pdf")
# 
# Halichondria_panicea_plot
# 
# Haliclona_oculata_plot <- ggplot(data = substrate,
#                                  aes(x = year, y = haliclona_oculata, #sub in species name
#                                      color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Haliclona oculata*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Haliclona_oculata_plot.pdf")
# 
# Haliclona_oculata_plot
# 
# Halocynthia_pyriformis_plot <- ggplot(data = substrate,
#                                       aes(x = year, y = halocynthia_pyriformis, #sub in species name
#                                           color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Halocynthia pyriformis*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Halocynthia_pyriformis_plot.pdf")
# 
# Halocynthia_pyriformis_plot
# 
# Hymedesmia_paupertas_plot <- ggplot(data = substrate,
#                                     aes(x = year, y = hymedesmia_sp, #sub in species name
#                                         color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Hymedesmia paupertas*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Hymedesmia_paupertas_plot.pdf")
# 
# Hymedesmia_paupertas_plot #not in any photo
# 
# Isodictya_palmata_plot <- ggplot(data = substrate,
#                                  aes(x = year, y = isodictya_spp, #sub in species name
#                                      color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Isodictya palmata*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Isodictya_palmata_plot.pdf")
# 
# Isodictya_palmata_plot
# 
# Leucosolenia_botryoides_plot <- ggplot(data = substrate,
#                                        aes(x = year, y = leucosolenia_spp, #sub in species name
#                                            color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Leucosolenia botryoides*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Leucosolenia_botryoides_plot.pdf")
# 
# Leucosolenia_botryoides_plot
# 
Lithothamnion_glaciale_plot <- ggplot(data = substrate,
                                       aes(x = year, y = lithothamnion_spp, #sub in species name
                                           color = site)) +
   geom_line()+
   labs(x = "Year",
        y = "Percent Cover of *Lithothamnion glaciale*")+ #sub in species name
   theme(axis.title.y = ggtext::element_markdown())

   ggsave("figures/Lithothamnion_glaciale_plot.pdf")
 
Lithothamnion_glaciale_plot
# 
# Metriduim_senile_plot <- ggplot(data = substrate,
#                                 aes(x = year, y = metridium_sub, #sub in species name
#                                     color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Metriduim senile*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Metriduim_senile_plot.pdf")
# 
# Metriduim_senile_plot
# 
# Modiolus_modiolus_plot <- ggplot(data = substrate,
#                                  aes(x = year, y = modiolus_modiolus, #sub in species name
#                                      color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Modiolus modiolus*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Modiolus_modiolus_plot.pdf")
# 
# Modiolus_modiolus_plot
# 
# Mogula_citrina_plot <- ggplot(data = substrate,
#                               aes(x = year, y = molgula_citrina, #sub in species name
#                                   color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Molgula citrina*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Mogula_citrina_plot.pdf")
# 
# Mogula_citrina_plot
# 
# Mytilus_edulis_plot <- ggplot(data = substrate,
#                               aes(x = year, y = mytilus_edulis, #sub in species name
#                                   color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Mytilus edulis*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Mytilus_edulis_plot.pdf")
# 
# Mytilus_edulis_plot
# 
# Peyssonnelia_rosenvingei_plot <- ggplot(data = substrate,
#                                         aes(x = year, y = peysonnelia, #sub in species name
#                                             color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Peysonnelia rosenvingei*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Peyssonnelia_rosenvingei_plot.pdf")
# 
# Peyssonnelia_rosenvingei_plot
# 
# Spirorbis_spirorbis_plot <- ggplot(data = substrate,
#                                    aes(x = year, y = spirorbis_spp, #sub in species name
#                                        color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Spirorbis spirorbis*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Spirorbis_spirorbis_plot.pdf")
# 
# Spirorbis_spirorbis_plot
# 
# Styela_clava_plot <- ggplot(data = substrate,
#                             aes(x = year, y = styela_sp, #sub in species name
#                                 color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Styela clava*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Styela_clava_plot.pdf")
# 
# Styela_clava_plot
# 
# Ectopleura_crocea_plot <- ggplot(data = substrate,
#                                  aes(x = year, y = tubularia_sub, #sub in species name
#                                      color = site)) +
#   geom_line()+
#   labs(x = "Year",
#        y = "Percent Cover of *Ectopleura crocea*")+ #sub in species name
#   theme(axis.title.y = ggtext::element_markdown())+  
#   ggsave("figures/Ectopleura_crocea_plot.pdf")
# 
# Ectopleura_crocea_plot
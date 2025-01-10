#' -----------------------------------------------
#' This script creates visualizations of timeseries
#' of species
#' -----------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)

sebens_substrate_proportion <-read_csv("data/sebens_substrate_proportion.csv")
View(sebens_substrate_proportion)

sebens_substrate_proportion <-
  sebens_substrate_proportion |>
  filter(year > 1970,
         month %in% c(6:9),
         angle == "vertical") |>
  mutate(year_cent = year - mean(year))

str(sebens_substrate_proportion)


#Use corrected names from coefs_with_indicies.csv

Alcyonium_siderium_plot <- ggplot(data = sebens_substrate_proportion,
       aes(x = year, y = alcyonium_sub, #sub in species name
           color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Alcyonium siderium*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Alcyonium_siderium_plot

Anomia_simplex_plot <- ggplot(data = sebens_substrate_proportion,
                              aes(x = year, y = anomia_spp, #sub in species name
                                  color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Anomia simplex*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Anomia_simplex_plot

Aplidium_glabrum_plot <- ggplot(data = sebens_substrate_proportion,
                                aes(x = year, y = aplidium_glabrum, #sub in species name
                                    color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Aplidium glabrum*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Aplidium_glabrum_plot

Balanus_balanus_plot <- ggplot(data = sebens_substrate_proportion,
                               aes(x = year, y = balanus_balanus, #sub in species name
                                   color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Balanus balanus*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Balanus_balanus_plot

Boltenia_echinata_plot <- ggplot(data = sebens_substrate_proportion,
                                 aes(x = year, y = boltenia_echinata, #sub in species name
                                     color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Boltenia echinata*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Boltenia_echinata_plot

Boltenia_ovifera_plot <- ggplot(data = sebens_substrate_proportion,
                                aes(x = year, y = boltenia_ovifera, #sub in species name
                                    color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Boltenia ovifera*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Boltenia_ovifera_plot

Botrylloides_violaceus_plot <- ggplot(data = sebens_substrate_proportion,
                                      aes(x = year, y = botrylloides_sp, #sub in species name
                                          color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Botrylloides violaceus*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Botrylloides_violaceus_plot

Botryllus_schlosseri_plot <- ggplot(data = sebens_substrate_proportion,
                                    aes(x = year, y = botryllus_schlosseri, #sub in species name
                                        color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Botryllus schlosseri*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Botryllus_schlosseri_plot

Clathromorphum_circumscriptum_plot <- ggplot(data = sebens_substrate_proportion,
                                        aes(x = year, y = clathromorphum_sp, #sub in species name
                                            color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Clathromorphum circumscriptum*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Clathromorphum_circumscriptum_plot

Cliona_celata_plot <- ggplot(data = sebens_substrate_proportion,
                             aes(x = year, y = cliona_spp, #sub in species name
                                 color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Cliona celata*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Cliona_celata_plot

Dendrodoa_carnea_plot <- ggplot(data = sebens_substrate_proportion,
                                aes(x = year, y = dendrodoa_carnea, #sub in species name
                                    color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Dendrodoa carnea*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Dendrodoa_carnea_plot

Didemnum_albidum_plot <- ggplot(data = sebens_substrate_proportion,
                                aes(x = year, y = didemnum_albidum, #sub in species name
                                    color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Didemnum albidum*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Didemnum_albidum_plot

Didemnum_vexillum_plot <- ggplot(data = sebens_substrate_proportion,
                                 aes(x = year, y = didemnum_vexillum, #sub in species name
                                     color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Didemnum vexillum*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Didemnum_vexillum_plot

Dilosoma_listerianum_plot <- ggplot(data = sebens_substrate_proportion,
                                    aes(x = year, y = diplosoma_listerianum, #sub in species name
                                        color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Diplosoma listerianum*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Dilosoma_listerianum_plot

Edwardsiella_lineata_plot <- ggplot(data = sebens_substrate_proportion,
                                    aes(x = year, y = edwardsiella_lineata, #sub in species name
                                        color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Edwardsiella lineata*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Edwardsiella_lineata_plot

Halichondria_panicea_plot <- ggplot(data = sebens_substrate_proportion,
                                    aes(x = year, y = halichondria_panicea, #sub in species name
                                        color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Halichondria panicea*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Halichondria_panicea_plot

Haliclona_oculata_plot <- ggplot(data = sebens_substrate_proportion,
                                 aes(x = year, y = haliclona_oculata, #sub in species name
                                     color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Haliclona oculata*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Haliclona_oculata_plot

Halocynthia_pyriformis_plot <- ggplot(data = sebens_substrate_proportion,
                                      aes(x = year, y = halocynthia_pyriformis, #sub in species name
                                          color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Halocynthia pyriformis*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Halocynthia_pyriformis_plot

Hymedesmia_paupertas_plot <- ggplot(data = sebens_substrate_proportion,
                                    aes(x = year, y = hymedesmia_sp, #sub in species name
                                        color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Hymedesmia paupertas*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Hymedesmia_paupertas_plot #not ever found

Isodictya_palmata_plot <- ggplot(data = sebens_substrate_proportion,
                                 aes(x = year, y = isodictya_spp, #sub in species name
                                     color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Isodictya palmata*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Isodictya_palmata_plot

Leucosolenia_botryoides_plot <- ggplot(data = sebens_substrate_proportion,
                                       aes(x = year, y = leucosolenia_spp, #sub in species name
                                           color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Leucosolenia botryoides*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Leucosolenia_botryoides_plot

Lithothamnion_glaciale_plot <- ggplot(data = sebens_substrate_proportion,
                                      aes(x = year, y = lithothamnion_spp, #sub in species name
                                          color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Lithothamnion glaciale*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Lithothamnion_glaciale_plot

Metriduim_senile_plot <- ggplot(data = sebens_substrate_proportion,
                                aes(x = year, y = metridium_sub, #sub in species name
                                    color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Metriduim senile*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Metriduim_senile_plot

Modiolus_modiolus_plot <- ggplot(data = sebens_substrate_proportion,
                                 aes(x = year, y = modiolus_modiolus, #sub in species name
                                     color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Modiolus modiolus*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Modiolus_modiolus_plot

Mogula_citrina_plot <- ggplot(data = sebens_substrate_proportion,
                              aes(x = year, y = molgula_citrina, #sub in species name
                                  color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Molgula citrina*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Mogula_citrina_plot

Mytilus_edulis_plot <- ggplot(data = sebens_substrate_proportion,
                              aes(x = year, y = mytilus_edulis, #sub in species name
                                  color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Mytilus edulis*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Mytilus_edulis_plot

Peyssonnelia_rosenvingei_plot <- ggplot(data = sebens_substrate_proportion,
                                        aes(x = year, y = peysonnelia, #sub in species name
                                            color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Peysonnelia rosenvingei*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Peyssonnelia_rosenvingei_plot

Spirorbis_spirorbis_plot <- ggplot(data = sebens_substrate_proportion,
                                   aes(x = year, y = spirorbis_spp, #sub in species name
                                       color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Spirorbis spirorbis*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Spirorbis_spirorbis_plot

Styela_clava_plot <- ggplot(data = sebens_substrate_proportion,
                            aes(x = year, y = styela_sp, #sub in species name
                                color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Styela clava*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Styela_clava_plot

Ectopleura_crocea_plot <- ggplot(data = sebens_substrate_proportion,
                                 aes(x = year, y = tubularia_sub, #sub in species name
                                     color = site)) +
  geom_line()+
  labs(x = "Year",
       y = "Percent Cover of *Ectopleura crocea*")+ #sub in species name
  theme(axis.title.y = ggtext::element_markdown())

Ectopleura_crocea_plot
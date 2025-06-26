#' -----------------------------------------------
#' This script makes a histogram of how species 
#' abundances have changed from pre-1985 to post
#' 2015
#' -----------------------------------------------


library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# load the data
source("scripts/load_data_and_provide_constants.R")

dict <- read_csv("data/co_occuring_species.csv")

substrate_long <- substrate_long |>
  rename(coefficients_species = species)


# First, translate indices species into coef species names
substrate_long_joined <- substrate_long |>
  left_join(dict) |>
  rename(species = coefficients_species) |>
  filter(!is.na(species)) |>
  relocate(species)


current_doms <- substrate_long_joined |>
  filter(year > 2015) |>
  group_by(gen_spp, area)|>
  summarise(Average = mean(proportion, na.rm = TRUE))|>
  arrange(desc(Average))|>
  rename(current_average = Average)|>
  filter(!is.na(gen_spp))

View(current_doms)

past_doms <- substrate_long_joined |>
  filter(year < 1985) |>
  group_by(gen_spp, area) |>
  summarise(Average = mean(proportion, na.rm = TRUE))|>
  arrange(desc(Average))|>
  rename(past_average = Average)|>
  filter(!is.na(gen_spp))

View(past_doms)

past_current_doms <- left_join(past_doms, current_doms) 

View(past_current_doms)

past_current_doms <- past_current_doms |>
  pivot_longer(cols = c("past_average", "current_average"),
               names_to = "Period",
               values_to = "Proportion")

colors <- c("turquoise","purple")
outcome_labels <- c("Current (2015-2020)","Past (1980-1985)")

doms_barchart <- past_current_doms |>
  ggplot() +
  geom_col(aes(x=gen_spp, y=Proportion, fill = Period), position = "dodge") +
  theme(axis.text.x = element_text(angle = -90, vjust= 0.1, hjust=0))+
  labs(x = "Species", y = "Proportion of Subsite coverage") +
  scale_fill_manual(values = colors,labels = outcome_labels)+
  facet_wrap(~area)

ggsave(glue::glue("figures/doms_barchart.jpg"))
  
DB_doms_barchart <- past_current_doms |>
  filter(area == "Dive Beach") |>
  ggplot() +
  geom_col(aes(x=gen_spp, y=Proportion, fill = Period), position = "dodge") +
  theme(axis.text.x = element_text(angle = -90, vjust= 0.1, hjust=0))+
  labs(title = "Dive Beach", x = "Species", y = "Proportion of Subsite coverage") +
  scale_fill_manual(values = colors,labels = outcome_labels)

ggsave(glue::glue("figures/DB_doms_barchart.jpg"))


HRI_doms_barchart <- past_current_doms |>
  filter(area == "Halfway Rock Inner") |>
  ggplot() +
  geom_col(aes(x=gen_spp, y=Proportion, fill = Period), position = "dodge") +
  theme(axis.text.x = element_text(angle = -90, vjust= 0.1, hjust=0))+
  labs(title = "Halfway Rock Inner", x = "Species", y = "Proportion of Subsite coverage") +
  scale_fill_manual(values = colors,labels = outcome_labels)

ggsave(glue::glue("figures/HRI_doms_barchart.jpg"))

#HRO didnt exist until 1981-1989
#HRO_doms_barchart <- past_current_doms |>
#  filter(area == "Halfway Rock Outer") |>
#  ggplot() +
#  geom_col(aes(x=gen_spp, y=Proportion, fill = Period), position = "dodge") +
#  theme(axis.text.x = element_text(angle = -90, vjust= 0.1, hjust=0))+
#  labs(title = "Halfway Rock Outer", x = "Species", y = "Proportion of Subsite coverage") +
#  scale_fill_manual(values = colors,labels = outcome_labels)

#ggsave(glue::glue("figures/HRO_doms_barchart.jpg"))


SHI_doms_barchart <- past_current_doms |>
  filter(area == "Shag Rocks Inner") |>
  ggplot() +
  geom_col(aes(x=gen_spp, y=Proportion, fill = Period), position = "dodge") +
  theme(axis.text.x = element_text(angle = -90, vjust= 0.1, hjust=0))+
  labs(title = "Shag Rocks Inner", x = "Species", y = "Proportion of Subsite coverage") +
  scale_fill_manual(values = colors,labels = outcome_labels)

ggsave(glue::glue("figures/SHI_doms_barchart.jpg"))


SHO_doms_barchart <- past_current_doms |>
  filter(area == "Shag Rocks Outer") |>
  ggplot() +
  geom_col(aes(x=gen_spp, y=Proportion, fill = Period), position = "dodge") +
  theme(axis.text.x = element_text(angle = -90, vjust= 0.1, hjust=0))+
  labs(title = "Shag Rocks Outer", x = "Species", y = "Proportion of Subsite coverage") +
  scale_fill_manual(values = colors,labels = outcome_labels)

ggsave(glue::glue("figures/SHO_doms_barchart.jpg"))

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
  group_by(gen_spp)|>
  summarise(Average = mean(proportion, na.rm = TRUE))|>
  arrange(desc(Average))|>
  rename(current_average = Average)|>
  filter(!is.na(gen_spp))

View(current_doms)

past_doms <- substrate_long_joined |>
  filter(year < 1985) |>
  group_by(gen_spp) |>
  summarise(Average = mean(proportion, na.rm = TRUE))|>
  arrange(desc(Average))|>
  rename(past_average = Average)|>
  filter(!is.na(gen_spp))

View(past_doms)

past_current_doms <- left_join(past_doms, current_doms) 

View(past_current_doms)

past_current_doms <- past_current_doms |>
  pivot_longer(cols = c("past_average", "current_average"),
               names_to = "period",
               values_to = "Proportion")

colors <- c("turquoise","purple")
outcome_labels <- c("Current (2015-2020)","Past (1979-1985)")

doms_barchart <- past_current_doms |>
  ggplot() +
  geom_col(aes(x=gen_spp, y=Proportion, fill = period), position = "dodge") +
  theme(axis.text.x = element_text(angle = -90, hjust=0))+
  labs(x = "Species", y = "Proportion of Subsite coverage") +
  scale_fill_manual(values = colors,labels = outcome_labels)
  

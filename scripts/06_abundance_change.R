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

current_doms <- substrate_long |>
  filter(year > 2015) |>
  group_by(species)|>
  summarise(Average = mean(proportion, na.rm = TRUE))|>
  arrange(desc(Average))|>
  rename(current_average = Average)

View(current_doms)

past_doms <- substrate_long |>
  filter(year < 1985) |>
  group_by(species) |>
  summarise(Average = mean(proportion, na.rm = TRUE))|>
  arrange(desc(Average))|>
  rename(past_average = Average)

View(past_doms)

past_current_doms <- left_join(past_doms, current_doms) 

View(past_current_doms)

past_current_doms <- past_current_doms |>
  pivot_longer(cols = c("past_average", "current_average"),
               names_to = "period",
               values_to = "Proportion")

doms_barchart <- past_current_doms |>
  ggplot() +
  geom_col(aes(x=species, y=Proportion, fill = period), position = "dodge")
  

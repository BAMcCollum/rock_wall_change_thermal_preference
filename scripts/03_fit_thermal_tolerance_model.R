#' -----------------------------------------------
#' This script fits and evaluates a model looking
#' at species thermal tolerance and change and
#' saves the model out for visualization
#' -----------------------------------------------

library(readr)
library(ggplot2)
theme_set(theme_classic(base_size = 12))
library(dplyr)
library(tidyr)
library(purrr)
library(performance)
library(car)
library(broom)
library(ggrepel)
library(gtsummary)

# load data and set themes
source("scripts/load_data_and_provide_constants.R")


coefs_with_indicies <- read_csv("data/coefs_with_indices.csv")
View (coefs_with_indicies)

ggplot(coefs_with_indicies, aes(BO21_tempmax_bdmean_min, estimate))+
  geom_point()+
  stat_smooth(method = "lm") #Ideally would like to color by functional group, depth strata

mod1 <- lm(estimate ~ BO21_tempmax_bdmean_mean, data = coefs_with_indicies)
mod2 <- lm(estimate ~ BO21_tempmax_bdmean_max, data = coefs_with_indicies)
mod3 <- lm(estimate ~ BO21_tempmax_bdmean_min, data = coefs_with_indicies)

check_model(mod3)
check_normality(mod1) #Warning: Non-normality of residuals detected (p = 0.026)
check_normality(mod1) |> plot()

anova(mod1)
Anova(mod3)
summary(mod1)
tidy(mod1)
glance(mod1)

get_mod_info <- function(mod){
  mod |> 
    tidy() |> filter(term != "(Intercept)") |>
    bind_cols(glance(mod) |> select(r.squared))
}

mod_list <- list(mod1, mod2, mod3)
purrr:::map(mod_list,
            get_mod_info) |>
  bind_rows()

ggplot(coefs_with_indicies, aes(BO21_tempmax_bdmean_mean, estimate, label = gen_spp))+
  geom_point()+
  stat_smooth(method = "lm")+
  geom_text_repel(size = 6)+
  labs(x = "Thermal Preference (Occupancy derived max temp at species mean depth) in (°C)",  
       y = "Coefficient of Change over 45 years",
       title ="Thermal Preference and Change in Abundance")#

ggsave("figures/coefs_with_indicies.pdf", 
       width = 40, height = 25, units = "cm")


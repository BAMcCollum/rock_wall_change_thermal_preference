#' -----------------------------------------------
#' This script fits and evaluates a model looking
#' at species thermal tolerance and change and
#' saves the model out for visualization
#' -----------------------------------------------

library(readr)
library(ggplot2)
theme_set(theme_classic(base_size = 18))
library(dplyr)
library(tidyr)
library(purrr)
library(performance)
library(car)
library(broom)
library(ggrepel)
library(gt)

# load data and set themes
source("scripts/load_data_and_provide_constants.R")


coefs_with_indicies <- read_csv("data/coefs_with_indices.csv")
View (coefs_with_indicies)


ggplot(coefs_with_indicies, aes(BO21_tempmax_bdmean_mean, estimate))+
  geom_point()+
  stat_smooth(method = "lm") #Ideally would like to color by functional group, depth strata

mod1 <- lm(estimate ~ BO21_tempmax_bdmean_mean, data = coefs_with_indicies)
mod2 <- lm(estimate ~ BO21_tempmax_bdmin_mean, data = coefs_with_indicies)
mod3 <- lm(estimate ~BO21_tempmax_bdmin_max, data = coefs_with_indicies)

#Mod1
check_model(mod1)
check_normality(mod1) 

anova(mod1)
Anova(mod1)
summary(mod1)
tidy(mod1)
glance(mod1)

#Mod2
check_model(mod2)
check_normality(mod2) 

anova(mod2)
Anova(mod2)
summary(mod2)
tidy(mod2)
glance(mod2)

#Mod3
check_model(mod3)
check_normality(mod3) 

anova(mod3)
Anova(mod3)
summary(mod3)
tidy(mod3)
glance(mod3)

get_mod_info <- function(mod){
  mod |> 
    tidy() |> filter(term != "(Intercept)") |>
    bind_cols(glance(mod) |> select(r.squared))
}

mod_list <- list(mod1, mod2, mod3)
layer_comp <- purrr:::map(mod_list,
            get_mod_info) |>
  bind_rows()

#make a better looking table
gt::gt(layer_comp)


#lm_eqn <- function(df){
#  m <- lm(estimate ~ BO21_tempmax_bdmin_mean, data = coefs_with_indicies);
#  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
#                   list(a = format(unname(coef(m)[1]), digits = 2),
#                        b = format(unname(coef(m)[2]), digits = 2),
#                        r2 = format(summary(m)$r.squared, digits = 3)))
#  as.character(as.expression(eq));
#}

ggplot(coefs_with_indicies, aes(BO21_tempmax_bdmin_mean, estimate, label = gen_spp))+
  geom_point()+
  stat_smooth(method = "lm")+
  geom_text_repel(size = 6, fontface = "italic")+
  #annotate("text", x = 22.5, y = 0.2, label = lm_eqn(coefs_with_indicies), parse = TRUE, colour = "red")+
  theme_classic(base_size = 20)+
  labs(x = "Average Thermal Maxima (Occupancy derived max temp at species min depth) in (°C)",  
       y = "Coefficient of Change over 42 years")+
  geom_hline(yintercept=0.0, linetype='dashed', linewidth = 1.5, colour='orange') +
  geom_vline(xintercept=17.4, linewidth = 2, colour='green') +
  geom_vline(xintercept=14.07, linewidth = 2, colour='turquoise')

ggsave("figures/coefs_with_indicies.jpg")

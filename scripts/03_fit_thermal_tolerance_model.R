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

coefs_with_indicies <- read_csv("data/coefs_with_indices.csv")

ggplot(coefs_with_indicies, aes(BO21_tempmax_bdmean_mean, estimate))+
  geom_point()+
  stat_smooth(method = "lm")

mod1 <- lm(estimate ~ BO21_tempmax_bdmean_mean, data = coefs_with_indicies)

check_model(mod1)
check_normality(mod1) #Warning: Non-normality of residuals detected (p = 0.026)
check_normality(mod1) |> plot()

anova(mod1)
Anova(mod1)
summary(mod1)
tidy(mod1)
glance(mod1)

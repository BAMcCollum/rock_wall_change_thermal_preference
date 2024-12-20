#This script gets coefficients of change in % cover over time on vertical surfaces for each of the sessile species 
#in the larger Ken Sebens long-term monitoring project. These data will be used in chapter 2 of my dissertation.

library(readr)
library(glmmTMB)
library(visreg)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(broom.mixed)
library(broom)


#For 1 species
substrate <- 
  read_csv("downloads/sebens_substrate_proportion (1).csv") |>
  filter(year > 1970,
         month %in% c(6:9),
         angle == "vertical") |>
  mutate(year_cent = year - mean(year))

View(substrate)

ggplot(data = substrate,
       aes(x = year, y = styela_sp, #sub in species name
           color = site)) +
  geom_line() + geom_point() +
  facet_wrap(vars(area))

#Model
mod <- glmmTMB(mytilus_edulis ~
                 year_cent  + area + (1|site %in% area),
               data = substrate,
               family = ordbeta(link = "logit"))

performance::check_predictions(mod) # look at that!
car::Anova(mod)
summary(mod)

#check mod
DHARMa::simulateResiduals(mod) |> DHARMa::plotQQunif()

# plot results
visreg(mod, xvar = "year_cent", scale = "response")


######################################################
#For multiple species coefficients

substrate_long <- substrate |>
  
  # pivot longer so attributes (i.e., species) are in rows  
  pivot_longer(alcyonium_sub : tubularia_sub,
               names_to = "species",
               values_to = "proportion") |>
  
  # fit a model to each attribute
  group_by(species) |>
  nest() |>
  mutate(mod = map(data, ~glmmTMB(proportion ~
                                    year_cent  + area + (1|site %in% area),
                                  family = ordbeta(link = "logit"), data = .x)), #.x is a placeholder for each nested df
         coef = map(mod, ~tidy(.x) |> filter(term == "year_cent"))) |>
  
  # great, get back the data
  unnest(coef) |>
  select(-data, -mod, -term) |>
  print (n = 63)

View (substrate_long)

write.csv(substrate_long,"~/Downloads/substrate_long.csv", row.names = FALSE)

#The species behaving badly:

#boltenia_ovifera
#botryllus_schlosseri
#cliona_spp
#halocynthia_pyriformis
#hymedesmia_sp
#molgula_citrina
#mytilus_edulis
#orange_sponge_crust
#scypha_sp
#styela_sp

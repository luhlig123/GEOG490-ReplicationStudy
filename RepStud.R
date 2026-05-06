#GEOG 490 - Spatial Demography
#Replication Study
#Date: 4/6/2026
#Author: Leo Uhlig

#This project will replicate the methods used by Brice B. Hanberry to create a
#standardized definition of urban populations. For this study, the focus will be
#on Seattle, Washington.
#-------------------------------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(ggplot2)


v20 <- load_variables(2020, "acs5")
view(v20)

#Loading in Seattle population data by tract
Seattle_pop <- get_acs(
  geography = "tract",
  state = "WA",
  county = c("King", "Snohomish", "Pierce"),
  variables = "B01001A_001",
  year = 2020,
  geometry = TRUE,
)

#Calculate tract area
Seattle_pop <- Seattle_pop %>%
  mutate(area_calculated = sf::st_area(geometry)) %>%
  mutate(area_calculated = area_calculated/1000) %>%
  mutate(density = area_calculated / estimate)

#test add
Seattle_pop <- Seattle_pop %>%
  mutate(density_class = case_when(
    density >= 1900 ~ "Urban High",
    density >= 800 ~ "Urban Low",
    density >= 550 ~ "Suburban High",
    density >= 250 ~ "Suburban low",
    density >= 0 ~ "Exurban"
  ))

view(Seattle_pop)

#initial visualization
ggplot(data = Seattle_pop, aes(fill = density_class)) +
  geom_sf(color = NA) +
  theme_void()




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

#Loading in Seattle population data by tract
Seattle_pop <- get_acs(
  geography = "tract",
  state = "WA",
  county = c("King", "Snohomish", "Pierce"),
  variables = "B01001A_001",
  year = 2020,
  geometry = TRUE,
)

view(Seattle_pop)

#test add
df <- Seattle_pop %>%
  mutate(density_class = case_when(
    estimate >= 1900 ~ "Urban High",
    estimate >= 800 ~ "Urban Low",
    estimate >= 550 ~ "Suburban High",
    estimate >= 250 ~ "Suburban low",
    estimate >= 0 ~ "Exurban"
  ))
view(df)

#initial visualization
ggplot(data = df, aes(fill = density_class)) +
  geom_sf(color = NA) +
  theme_void()




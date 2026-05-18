#GEOG 490 - Spatial Demography
#Replication Study
#Date: 4/6/2026
#Author: Leo Uhlig

#This project will replicate the methods used by Brice B. Hanberry to create a
#standardized definition of urban populations. For this study, the focus will be
#on the Seattle metropolitan area.
#-------------------------------------------------------------------------------
library(tidycensus)
library(tidyverse)
library(ggplot2)
library(sf)


#Loading in Seattle population data by tract
Seattle_pop <- get_acs(
  geography = "tract",
  state = "WA",
  county = c("King", "Snohomish", "Pierce"),
  variables = "B01003_001",
  year = 2020,
  survey = 'acs5',
  geometry = TRUE,
)

#Calculate tract area
Seattle_pop <- Seattle_pop %>%
  mutate(area_calculated = st_area(geometry)) %>%
  mutate(area_calculated = as.numeric(area_calculated)) %>%
  mutate(area_calculated = area_calculated/1000000) %>%
  mutate(density = estimate / area_calculated)


#Creating density class variable
Seattle_pop <- Seattle_pop %>%
  mutate(density_class = case_when(
    density > 1900 ~ "Urban High",
    density >= 800 ~ "Urban Low",
    density >= 550 ~ "Suburban High",
    density >= 250 ~ "Suburban Low",
    density > 0 ~ "Exurban"
  ))


#Visualization of Seattle population density groups by tract
pop_density <- ggplot(data = Seattle_pop, aes(fill = density_class)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_d() +
  labs(title = "Seattle Metro Area Population Density Groups by Tract")
ggsave("Seattle_pop_density.png", pop_density)


#-------------------------------------------------------------------------------
#MAP OF POPULATION BELOW POVERTY LINE

#Poverty data
Seattle_metro_poverty <- get_acs(
  geography = "tract",
  state = "WA",
  county = c("King", "Snohomish", "Pierce"),
  variables = c(pop = "B17001_001", pop_pov = "B17001_001"),
  year = 2020,
  survey = "acs5",
  geometry = TRUE,
  output = "wide"
) %>%
  rename(pop = popE) %>%
  rename(pop_pov = popM) %>%
  mutate(percent_pov = (pop_pov/pop) * 100)

#Plot poverty data
Seattle_poverty <- ggplot(data = Seattle_metro_poverty, aes(fill = pop_pov)) +
  geom_sf(color = NA) +
  theme_void() +
  scale_fill_viridis_c() +
  labs(
    title = "Seattle Metro Area Population Under the Poverty Line"
  )
ggsave("Seattle_poverty.png", Seattle_poverty)


#Poverty By Percent
#ggplot(data = Seattle_metro_poverty, aes(fill = percent_pov)) +
#  geom_sf(color = NA) +
#  theme_void()

#-------------------------------------------------------------------------------
#GRAPH OF MEDIAN HOUSEHOLD INCOME

#Seattle metro median income data
Seattle_metro_income <- get_acs(
  geography = "tract",
  state = "WA",
  county = c("King", "Snohomish", "Pierce"),
  variables = "B19013_001",
  year = 2020,
  survey = 'acs5',
  geometry = TRUE,
) %>%
  rename(Median_income = estimate)

#Median income histogram
Seattle_median_income <- ggplot(data = Seattle_metro_income, aes(x = Median_income)) +
  geom_histogram(fill = "lightblue", color = "blue") +
  labs(
    title = "Seattle Metro Area Median Income Distribution by Tract"
  )
ggsave("Seattle_median_income.png", Seattle_median_income)

#-------------------------------------------------------------------------------
#POPULATION PYRAMID

#Population by age
Seattle_pop_by_age <- get_acs(
  geography = "tract",
  variables = c(
    age0_5m_ = "B01001_003",
    age5_9m_ = "B01001_004",
    age10_14m_ = "B01001_005",
    age15_19m = c("B01001_006", "B01001_007"),
    age20_24m = c("B01001_008", "B01001_009", "B01001_010"),
    age25_29m_ = "B01001_011",
    age30_34m_ = "B01001_012",
    age35_39m_ = "B01001_013",
    age40_44m_ = "B01001_014",
    age45_49m_ = "B01001_015",
    age50_54m_ = "B01001_016",
    age55_59m_ = "B01001_017",
    age60_64m = c("B01001_018", "B01001_019"),
    age65_69m = c("B01001_020", "B01001_021"),
    age70_74m_ = "B01001_022",
    age75_79m_ = "B01001_023",
    age80_84m_ = "B01001_024",
    age85_and_olderm_ = "B01001_025",
    age0_5f_ = "B01001_027",
    age5_9f_ = "B01001_028",
    age10_14f_ = "B01001_029",
    age15_19f = c("B01001_030", "B01001_031"),
    age20_24f = c("B01001_032", "B01001_033", "B01001_034"),
    age25_29f_ = "B01001_035",
    age30_34f_ = "B01001_036",
    age35_39f_ = "B01001_037",
    age40_44f_ = "B01001_038",
    age45_49f_ = "B01001_039",
    age50_54f_ = "B01001_040",
    age55_59f_ = "B01001_041",
    age60_64f = c("B01001_042", "B01001_043"),
    age65_69f = c("B01001_044", "B01001_045"),
    age70_74f_ = "B01001_046",
    age75_79f_ = "B01001_047",
    age80_84f_ = "B01001_048",
    age85_and_olderf_ = "B01001_049"
  ),
  state = "WA",
  county = c("King", "Snohomish", "Pierce"),
  survey = "acs5",
  year = 2020,
)


#Joining age/sex data with population density information
full_seattle_pop <- full_join(Seattle_pop, Seattle_pop_by_age, by = "GEOID")

#subset by urban and suburban, then group by age brackets

#urban
Seattle_urban <- full_seattle_pop %>%
  filter(density_class == c("Urban High", "Urban Low")) %>%
  rename(age_group = variable.y) %>%
  rename(age_count = estimate.y) %>%
  group_by(age_group) %>%
  summarise(total_age_count = sum(age_count)) %>%
  mutate(sex = ifelse(str_detect(age_group, "m"), "M", "F"))

Seattle_urban_filtered <- Seattle_urban %>%
  mutate(total_age_count = ifelse(sex == "M", -total_age_count, total_age_count))

Seattle_urban_filtered$age_group <- sub("..$", "",
                                           Seattle_urban_filtered$age_group)

Seattle_urban_pyramid <- ggplot(
  Seattle_urban_filtered,
  aes(x = total_age_count, y = age_group, fill = sex)) +
  geom_col() +
  labs(
    title = "Seattle Population by Age in Urban Areas"
  )
ggsave("Seattle_urban_pyramid.png", Seattle_urban_pyramid)

#suburban
Seattle_suburban <- full_seattle_pop %>%
  filter(density_class == c("Suburban High", "Suburban Low")) %>%
  rename(age_group = variable.y) %>%
  rename(age_count = estimate.y) %>%
  group_by(age_group) %>%
  summarise(total_age_count = sum(age_count)) %>%
  mutate(sex = ifelse(str_detect(age_group, "m"), "M", "F"))

Seattle_suburban_filtered <- Seattle_suburban %>%
  mutate(total_age_count = ifelse(sex == "M", -total_age_count, total_age_count))

Seattle_suburban_filtered$age_group <- sub("..$", "",
                                           Seattle_suburban_filtered$age_group)

Seattle_suburban_pyramid <- ggplot(
  Seattle_suburban_filtered,
  aes(x = total_age_count, y = age_group, fill = sex)) +
  geom_col() +
  labs(
    title = "Seattle Population by Age in Suburban Areas"
  )
ggsave("Seattle_suburban_pyramid.png", Seattle_suburban_pyramid)


## cleaning spatial data

## libraries
library(tidyverse)
library(ggplot2)
library(sf)
library(tigris)
library(censusxy)

## load data
mcmf <- read_tsv("data/convert_MCMF_ALL_TIME_DATA.csv") %>%
  janitor::clean_names()

## IMPUTE SOME LAT/LON MISSINGNESS WITH CENSUS API

## create a df to be populated
## by address and cxy_single results
df <- censusxy::cxy_single("3700 Lindell Blvd", "St. Louis", "MO", 63139) %>%
  mutate(address = "HAHA") %>%
  colnames() %>% 
  purrr::map_dfc(setNames, object = list(character()))

## finding data with addresses but no points
mcmf2 <- mcmf %>%
  filter(!is.na(address)) %>%
  filter(!is.na(city)) %>%
  filter(!is.na(zipcode)) %>%
  filter(!is.na(state)) %>%
  ## maybe could be more thorough than this above and try parsing through
  filter(is.na(latitude)) %>% 
  select(address, city, zipcode, state) %>%
  distinct()

## finding lat/lon from address
for (i in 1:nrow(mcmf2)){
  cxy_out <- cxy_single(mcmf2[i, ]$address, mcmf2[i, ]$city, mcmf2[i, ]$state, mcmf2[i, ]$zipcode)
  if (is.null(cxy_out)){
    next
  }
  cxy_out <- cxy_out %>% 
    mutate(address = mcmf2[i, ]$address) %>%
    mutate_all(as.character)
  df <- bind_rows(df, cxy_out)  
}

######## put this into the function
df2 <- df %>%
  select(address,
         coordinates.x,
         coordinates.y)

mcmf_impute <- mcmf %>%
  filter(meeting_type == "face_to_face") %>%
  filter(!is.na(address)) %>%
  filter(!is.na(city)) %>%
  filter(!is.na(zipcode)) %>%
  filter(!is.na(state)) %>%
  filter(is.na(latitude)) %>% 
  left_join(df2)

mcmf_map <- mcmf %>%
  filter(!is.na(latitude)) %>%
  bind_rows(mcmf_impute) %>%
  mutate(latitude = as.character(latitude)) %>%
  mutate(longitude = as.character(longitude)) %>%
  mutate(latitude = dplyr::coalesce(latitude, coordinates.x)) %>%
  mutate(longitude = dplyr::coalesce(longitude, coordinates.y)) %>%
  filter(!is.na(latitude)) %>%
  filter(!is.na(longitude)) %>%
  select(-c(starts_with("coordinates")))

## spatial join points with tracts

illinois <- tidycensus::get_acs(state = "IL", geography = "tract",
                                variables = "B19013_001", geometry = TRUE)

map <- read_csv("data/mcmf_map.csv") %>%
  janitor::clean_names() %>%
  # convert multipolygons
  st_as_sf(wkt = "the_geom",
           # within illinois coordinate reference system
           crs = st_crs(illinois))

mcmf_map2 <- mcmf_map %>%
  # convert longitude, latitude to one column of points
  st_as_sf(coords = c("longitude", "latitude"),
  # within illinois coordinate reference system
  crs = st_crs(illinois))

mcmf_sp <- st_join(map, mcmf_map2)

mcmf_cat <- mcmf_sp %>%
  ##select(community, category_name) %>%
  group_by(community, category_name) %>%
  dplyr::summarise(n = n())

ggplot() +
  geom_sf(data = mcmf_cat, aes(fill = n)) +
  facet_wrap("category_name")

ggplot() +
  geom_sf(data = mcmf_sp)

save(mcmf_map2, mcmf_sp, map, file = "map_cleaning.rda")

## eda maps

## load packages
library(ggplot2)
library(tidyverse)
library(sf)
library(leaflet)
library(raster)

## load data
load("map_cleaning.rda")

## community & category counts
eda_counts <- mcmf_sp %>%
  group_by(community, category_name) %>%
  dplyr::summarise(n = n())

ggplot() +
  geom_sf(data = eda_counts, aes(fill = n)) +
  facet_wrap("category_name")

## pays participants?
eda_paypct <- mcmf_sp %>%
  group_by(community, category_name) %>%
  summarise(pct = sum())

## save for eda
save(eda_counts, file = "eda_maps.rda")
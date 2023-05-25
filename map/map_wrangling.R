library(shiny)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(gtExtras)
library(sf)
library(tidycensus)
library(mapview)
library(leaflet)
library(sp)


load("map_cleaning.rda")

eda_counts <- mcmf_sp %>%
  group_by(community, category_name) %>%
  summarise(n = n()) %>% 
  mutate(category_name = str_replace_all(category_name, '\\.', '')) %>% 
  mutate(general_category = case_when(
    category_name %in% c("Music & Art", "Performance", "Sports + Wellness", "Nature", "Food") ~ "Leisure & Arts",
    category_name %in% c("Academic Support", "Math", "Reading & Writing", "Science", "Science & Math","Social Studies", "Teaching") ~ "Academics",
    category_name %in% c("Building & Fixing Things", "Computers", "Digital Media", "Managing Money", "Law", "Work + Career") ~ "Professional Skill Building",
    category_name %in% c("Helping Your Community", "Transportation", "Customer/Human Service", "Healthcare") ~ "Community Service"))

gencat_count <- eda_counts %>%
  group_by(community, general_category) %>%
  summarize(n = sum(n))

save(eda_counts, gencat_count, map, file = "map_counts.rda")

# # load in data
# data <- read_tsv("convert_MCMF_ALL_TIME_DATA.csv", show_col_types = FALSE) %>% 
#   clean_names() %>% 
#   # removing irrelevant columns
#   select(-c("index_row", "logo_url", "online_address", "program_url", 
#             "registration_url","contact_name", "contact_email", 
#             "contact_phone")) %>%
#   # renaming duplicate category names
#   mutate(category_name = str_replace(category_name, '&', 'And'),
#          category_name = str_replace_all(category_name, '\\.', '')) %>% 
#   # removing programs with min_age over 25
#   filter(min_age < 25)

##### EXTRA WRANGLING
# 
# removed_programs <- data %>%
#   filter((grepl("geocoding", description, ignore.case = TRUE) |
#              grepl("test program", program_name, ignore.case = TRUE)))
# 
# 
# # data w/ column for length of opportunity
# time <- data %>%
#   mutate(start_date = as.Date(start_date, format = "%m/%d/%y"),
#          end_date = parse_date_time(end_date, orders = c("%m/%d/%y")),
#          program_length = as.numeric(difftime(end_date, start_date, units = "days"))) %>% 
#   mutate(program_length = ifelse(program_length == 0, 1, program_length))
# 
# # data w/ column for season of each event based on month of start_date
# seasons <- data %>%
#   mutate(new_start_date = parse_date_time(start_date, orders = c("%m/%d/%y")),
#          program_season = case_when(
#            month(new_start_date) %in% c(3,4,5) ~ 'spring',
#            month(new_start_date) %in% c(6,7,8) ~ 'summer',
#            month(new_start_date) %in% c(9,10,11) ~ 'fall',
#            month(new_start_date) %in% c(12,1,2) ~ 'winter',
#          )) %>% 
#   select(-new_start_date)

# addressing data points with multiple categories
# data w/ categories for each program based on id
# cat_duplicates <- data %>%
#   group_by(id) %>%
#   summarize(category_name = paste0(category_name, collapse = ", ")) %>%
#   mutate(category_name = strsplit(category_name, ", "))
# 
# data_nodup <- data %>%
#   select(-category_name) %>%
#   distinct(id, .keep_all = TRUE) %>%
#   inner_join(cat_duplicates, by = c("id"))

## community count
# eda_counts <- mcmf_sp %>%
#   group_by(community, category_name) %>%
#   summarise(n = n()) %>% 
#   mutate(category_name = str_replace_all(category_name, '\\.', '')) %>% 
#   mutate(general_category = case_when(
#     category_name %in% c("Music & Art", "Performance", "Sports + Wellness", "Nature", "Food") ~ "Leisure & Arts",
#     category_name %in% c("Academic Support", "Math", "Reading & Writing", "Science", "Science & Math","Social Studies", "Teaching") ~ "Academics",
#     category_name %in% c("Building & Fixing Things", "Computers", "Digital Media", "Managing Money", "Law", "Work + Career") ~ "Professional Skill Building",
#     category_name %in% c("Helping Your Community", "Transportation", "Customer/Human Service", "Healthcare") ~ "Community Service"))
# 
# gencat_count <- eda_counts %>%
#   group_by(community, general_category) %>%
#   summarize(n = sum(n))

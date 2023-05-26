library(tidyverse)

load("map_cleaning.rda")
list_priority_areas = c("Austin", "North Lawndale", "Humboldt Park", 
                        "East Garfield Park", "Englewood", "Auburn Gresham",
                        "West Garfield Park", "Roseland", "Greater Grand Crossing",
                        "West Englewood", "South Shore", "New City", "Chicago Lawn",
                        "South Lawndale", "West Pullman"
)

eda_counts <- mcmf_sp %>%
  mutate(program_provides_free_food = replace_na(program_provides_free_food, FALSE)) %>%
  mutate(category_name = str_replace_all(category_name, '\\.', '')) %>% 
  mutate(general_category = case_when(
    category_name %in% c("Music & Art", "Performance", "Sports + Wellness", "Nature", "Food") ~ "Leisure & Arts",
    category_name %in% c("Academic Support", "Math", "Reading & Writing", "Science", "Science & Math","Social Studies", "Teaching") ~ "Academics",
    category_name %in% c("Building & Fixing Things", "Computers", "Digital Media", "Managing Money", "Law", "Work + Career") ~ "Professional Skill Building",
    category_name %in% c("Helping Your Community", "Transportation", "Customer/Human Service", "Healthcare") ~ "Community Service")) %>%
  mutate(min_grade = case_when(
    min_age <= 5 ~ 0, min_age == 6 ~ 1, min_age == 7 ~ 2, min_age == 8 ~ 3,
    min_age == 9 ~ 4, min_age == 10 ~ 5, min_age == 11 ~ 6, min_age == 12 ~ 7,
    min_age == 13 ~ 8, min_age == 14 ~ 9, min_age == 15 ~ 10, min_age == 16 ~ 11,
    min_age == 17 ~ 12, min_age >= 18 ~ 13),
    max_grade = case_when(
      max_age <= 5 ~ 0, max_age == 6 ~ 1, max_age == 7 ~ 2, max_age == 8 ~ 3,
      max_age == 9 ~ 4, max_age == 10 ~ 5, max_age == 11 ~ 6, max_age == 12 ~ 7,
      max_age == 13 ~ 8, max_age == 14 ~ 9, max_age == 15 ~ 10, max_age == 16 ~ 11,
      max_age == 17 ~ 12, max_age >= 18 ~ 13)) %>%
  filter(!is.na(general_category))


## execute within shiny app for grade level implementation
eda_counts2 <- eda_counts
gencat_count <- as.data.frame(eda_counts) %>%
  filter(min_grade <= 1 | max_grade >= 13) %>%
  group_by(community, general_category) %>%
  summarize(n = n(),
            free_food = sum(program_provides_free_food)) 

gencat_count <- gencat_count %>%
  group_by(community, general_category) %>%
  summarize(n = sum(n),
            free_food = sum(free_food)) %>% 
  inner_join(as.data.frame(eda_counts) %>% select(the_geom, community) %>% distinct(), by = "community")

gencat_count <- pivot_wider(data = as.data.frame(gencat_count),
            names_from = general_category,
            values_from = c(n, free_food, the_geom)) %>%
  select(-c(
            `the_geom_Leisure & Arts`,
            `the_geom_Professional Skill Building`,
            `the_geom_Community Service`)) %>%
  rename("the_geom" = "the_geom_Academics") %>%
  mutate(n_Academics = replace_na(n_Academics, 0),
         `n_Community Service` = replace_na(`n_Community Service`, 0),
         `n_Professional Skill Building` = replace_na(`n_Professional Skill Building`, 0),
         `n_Leisure & Arts` = replace_na(`n_Leisure & Arts`, 0),
         free_food_Academics = replace_na(free_food_Academics, 0),
         `free_food_Community Service` = replace_na(`free_food_Community Service`, 0),
         `free_food_Professional Skill Building` = replace_na(`free_food_Professional Skill Building`, 0),
         `free_food_Leisure & Arts` = replace_na(`free_food_Leisure & Arts`, 0)) %>%
  mutate(n_sum = n_Academics + `n_Community Service` +
           `n_Professional Skill Building` + `n_Leisure & Arts`,
         free_food_sum = free_food_Academics + `free_food_Community Service` +
           `free_food_Professional Skill Building` + `free_food_Leisure & Arts`) %>%
  mutate(n_prop_Academics = n_Academics / n_sum,
         `n_prop_Community Service` = `n_Community Service` / n_sum,
         `n_prop_Leisure & Arts` = `n_Leisure & Arts` / n_sum,
         `n_prop_Professional Skill Building` = `n_Professional Skill Building` / n_sum,
         free_food_prop_Academics = free_food_Academics / n_Academics,
         `free_food_prop_Community Service` = `free_food_Community Service` / `n_Community Service`,
         `free_food_prop_Leisure & Arts` = `free_food_Leisure & Arts` / `n_Leisure & Arts`,
         `free_food_prop_Professional Skill Building` = `free_food_Professional Skill Building` / `n_Professional Skill Building`) %>%
  mutate(free_food_prop_Academics = replace_na(free_food_prop_Academics, 0),
         `free_food_prop_Community Service` = replace_na(`free_food_prop_Community Service`, 0),
         `free_food_prop_Leisure & Arts` = replace_na(`free_food_prop_Leisure & Arts`, 0),
         `free_food_prop_Professional Skill Building` = replace_na(`free_food_prop_Professional Skill Building`, 0)) %>%
  mutate(priority = tolower(community) %in% tolower(list_priority_areas))

supp_data <- read_csv("data/ara.csv") %>% 
  inner_join(as.data.frame(eda_counts) %>% select(the_geom, community) %>% distinct(), 
             by = "community") %>%
  mutate(priority = tolower(community) %in% tolower(list_priority_areas)) %>%
  st_as_sf()

save(eda_counts, supp_data, file = "map/fixed_map_counts.rda")

supp_data %>%
  ggplot() +
  geom_sf(aes(fill = supp_data$college_enrollment),
          lwd = ifelse(supp_data$priority == TRUE, .9, .1)) +
  scale_fill_gradient(name = "Proportion", low = "white", high = "#6fbee6") +
  theme_void()


library(tidyverse)

load("map_cleaning.rda")

eda_counts <- mcmf_sp %>%
  mutate(program_provides_free_food = replace_na(program_provides_free_food, FALSE)) %>%
  group_by(community, category_name) %>%
  summarise(n = n(),
            free_food = sum(program_provides_free_food)) %>% 
  mutate(category_name = str_replace_all(category_name, '\\.', '')) %>% 
  mutate(general_category = case_when(
    category_name %in% c("Music & Art", "Performance", "Sports + Wellness", "Nature", "Food") ~ "Leisure & Arts",
    category_name %in% c("Academic Support", "Math", "Reading & Writing", "Science", "Science & Math","Social Studies", "Teaching") ~ "Academics",
    category_name %in% c("Building & Fixing Things", "Computers", "Digital Media", "Managing Money", "Law", "Work + Career") ~ "Professional Skill Building",
    category_name %in% c("Helping Your Community", "Transportation", "Customer/Human Service", "Healthcare") ~ "Community Service"))

gencat_count <- eda_counts %>%
  group_by(community, general_category) %>%
  summarize(n = sum(n),
            free_food = sum(free_food))

gencat_count <- pivot_wider(data = as.data.frame(gencat_count),
            names_from = general_category,
            values_from = c(n, free_food, the_geom)) %>%
  select(-c(n_NA, 
            free_food_NA,
            `the_geom_Leisure & Arts`, 
            `the_geom_Professional Skill Building`,
            `the_geom_Community Service`, the_geom_NA)) %>%
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
         `free_food_prop_Professional Skill Building` = replace_na(`free_food_prop_Professional Skill Building`, 0))

save(eda_counts, gencat_count, file = "map/fixed_map_counts.rda")


library(tidyverse)

data <- read_tsv("data/convert_MCMF_ALL_TIME_DATA.csv", show_col_types = FALSE) %>% 
  clean_names() %>% 
  select(-c("index_row", "logo_url", "online_address", "program_url", 
            "registration_url","contact_name", "contact_email", 
            "contact_phone")) %>%
  mutate(category_name = str_replace(category_name, '&', 'And'),
         category_name = str_replace_all(category_name, '\\.', '')) %>% 
  filter(min_age < 25,
         meeting_type == "online") %>%
  mutate(general_category = case_when(
    category_name %in% c("Music & Art", "Performance", "Sports + Wellness", "Nature", "Food") ~ "Leisure & Arts",
    category_name %in% c("Academic Support", "Math", "Reading & Writing", "Science", "Science & Math","Social Studies", "Teaching") ~ "Academics",
    category_name %in% c("Building & Fixing Things", "Computers", "Digital Media", "Managing Money", "Law", "Work + Career") ~ "Professional Skill Building",
    category_name %in% c("Helping Your Community", "Transportation", "Customer/Human Service", "Healthcare") ~ "Community Service"))

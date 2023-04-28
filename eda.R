library(tidyverse)
library(skimr)

## load data
dat = read_tsv("data/convert_MCMF_ALL_TIME_DATA.csv") 
dat

list_priority_areas = c("Austin", "North Lawndale", "Humboldt Park", 
                       "East Garfield Park", "Englewood", "Auburn Gresham",
                       "West Garfield Park", "Roseland", "Greater Grand Crossing",
                       "West Englewood", "South Shore", "New City", "Chicago Lawn",
                       "South Lawndale", "West Pullman"
)


## skim data
skim_without_charts(dat)


## data cleaning and mutations
# clean names, select out unnecessary features, filter out min_age < 25 data
dat <- dat %>%
  janitor::clean_names() %>%
  select(-c("index_row", "logo_url", "online_address", "program_url", 
            "registration_url","contact_name", "contact_email", 
            "contact_phone")) %>%
  filter(min_age<25) 

dat2 <- dat 

# addressing duplicate data points
  # saved duplicate category names into list of strings
category_dat <- dat %>%
  group_by(id) %>%
  summarize(category_name = paste0(category_name, collapse = ", ")) %>%
  mutate(category_name = strsplit(category_name, ", "))
dat <- dat %>%
  select(-category_name) %>%
  distinct(id, .keep_all = TRUE) %>%
  inner_join(category_dat, by = c("id"))
  
# take out days of the week columns?

# number of categories
dat <- dat %>%
  mutate(num_categories = lengths(category_name))

# number of days between start of registration and end of registration
dat %>%
  mutate(registration_open = as.Date(registration_open, 
                                     format = "%m/%d/%y"),
         registration_deadline = as.Date(registration_open, 
                                         format = "%m/%d/%y")) %>%
  mutate(days_to_register = as.numeric(registration_deadline - registration_open))
  # All of the registration_open and registration_deadline data are the same
  # meaningless

# whether the region is on priority list
dat <- dat %>%
  mutate(priority = tolower(geographic_cluster_name) %in% tolower(list_priority_areas))

# age range
dat <- dat %>%
  mutate(age_range = max_age - min_age)

# age range list
age_dat <- dat %>%
  mutate(age_list = toString(min_age:max_age)) %>%
  select(age_list)

age <- data.frame(matrix(NA,    
                          nrow = 73982,
                          ncol = 1)) %>%
  rename(age_list = matrix.NA..nrow...73982..ncol...1.)

for (ii in 1:nrow(dat)) {
  age_row <- age[ii,]
  age_row[0] = dat[ii,][7] - dat[ii,][6]
}

age <- data.frame(matrix(NA,    
                         nrow = 3,
                         ncol = 1)) %>%
  rename(age_list = matrix.NA..nrow...3..ncol...1.)
dat <- data.frame("min_age" = c(1,2,3), "max_age" = c(8,9, 11))
for (ii in 1:nrow(dat)) {
  age[ii, "age_list"] <- dat[ii,][2] - dat[ii,][1]
}

## Visualization
# dist. of regions with priority highlighted
ggplot(dat %>% filter(!is.na(geographic_cluster_name)), 
       mapping = aes(fct_infreq(geographic_cluster_name), fill = priority)) +
  geom_bar() + 
  labs(title = "Distribution of Regions",
       x = "Region Name",
       y = "Count",
       fill = "Priority Region") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 5, 
                                   angle = 90, 
                                   hjust = 0.95)) + 
  scale_fill_manual(values=c("#999999", "#56B4E9"))

# dist. of categories
ggplot(dat2 %>% filter(!is.na(category_name)), 
       mapping = aes(fct_infreq(category_name))) +
  geom_bar() +
  labs(title = "Distribution of Categories",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6, angle = 90, hjust = 0.95)) 



  
ggplot(dat, aes(min_age)) +
  geom_histogram()
ggplot(dat, aes(max_age)) +
  geom_histogram()

## EDA To-do
# skim, issues
# simple map (category and age groups in each region)
# seasonal analysis
# transport data?
# univariate distribution


# Figure out region using lon/lat or address (convert to lon/lat first)

# make map with density of programs in each region (geographic_cluster_name)
# could make it interactive by allowing user to filter with program category
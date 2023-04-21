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

## data cleaning
dat <- dat %>%
  janitor::clean_names() %>%
  select(-"index_row") %>%
  filter(min_age<25) 

## visualization
# make map with density of programs in each region (geographic_cluster_name)
# could make it interactive by allowing user to filter with program category

ggplot(dat, mapping = aes(fct_infreq(geographic_cluster_name))) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 3)) 
  # ADD color for ones that are in priority list

ggplot(dat, mapping = aes(fct_infreq(category_name))) +
  geom_bar() +
  theme(axis.text.x = element_text(size = 4)) 


# figure out a way to look at distribution of ages within the range
# (combine min_age and max_age)
age_dat <- dat %>%
  mutate(age_range = c(max_age - min))
  
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



# Make visualizations for EDA
# Clean data (repeats from multiple categories, how to deal with missing data,'
# take out unnecessary columns)

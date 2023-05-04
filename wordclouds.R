## wordclouds

## load packages
library(tidyverse)
library(ggplot2)
library(wordcloud)
library(spacyr)

## load data 
mcmf <- read_tsv("data/convert_MCMF_ALL_TIME_DATA.csv") %>%
  janitor::clean_names()

## PARSING AND LEMMATIZING TEXT
### for wordcloud production

## general lemmatize data function (for use in lemmatize_data)
parse_tokens <- function(doc) {
  
  spacy_parse(doc, pos = TRUE, entity = FALSE) %>% 
    filter(pos %in% c("NOUN", "VERB")) %>% 
    pull(lemma) %>% 
    str_c(collapse = " ")
}

# lemmatize "text" column of a given data frame
lemmatize_data <- function(df) {
  spacy_initialize(model = "en_core_web_sm")
  
  out_df <- df %>% 
    group_by(text) %>% 
    mutate(lemmas = parse_tokens(text)) %>% 
    ungroup(text)
  
  spacy_finalize()
  
  return(out_df)
}

desc_lemmas <- mcmf %>%
  mutate(text = description) %>%
  lemmatize_data()


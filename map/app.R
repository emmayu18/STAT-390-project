# My Chi My Future Shiny Application ---------------------------------------


# load packages that will be used for the application
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

# load in data ------------------------------------------------------------------
load("map_cleaning.rda")
load("fixed_map_counts.rda")

data <- read_tsv("convert_MCMF_ALL_TIME_DATA.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  # removing irrelevant columns
  select(-c("index_row", "logo_url", "online_address", "program_url",
            "registration_url","contact_name", "contact_email",
            "contact_phone")) %>%
  # renaming duplicate category names
  mutate(category_name = str_replace(category_name, '&', 'And'),
         category_name = str_replace_all(category_name, '\\.', '')) %>%
  # removing programs with min_age over 25
  filter(min_age < 25)

seasons <- data %>%
  mutate(new_start_date = parse_date_time(start_date, orders = c("%m/%d/%y")),
         program_season = case_when(
           month(new_start_date) %in% c(3,4,5) ~ 'spring',
           month(new_start_date) %in% c(6,7,8) ~ 'summer',
           month(new_start_date) %in% c(9,10,11) ~ 'fall',
           month(new_start_date) %in% c(12,1,2) ~ 'winter'),
         year = year(new_start_date)) %>%
  mutate(category_name = case_when(
    category_name %in% c("Music & Art", "Music And Art", "Performance", "Sports + Wellness", "Nature", "Food") ~ "Leisure & Arts",
    category_name %in% c("Academic Support", "Math", "Reading & Writing", "Reading And Writing", "Science", "Science & Math", "Science And Math",
                         "Social Studies", "Teaching") ~ "Academics",
    category_name %in% c("Building And Fixing Things", "Building & Fixing Things", "Computers", "Digital Media", "Managing Money",
                         "Law", "Work + Career") ~ "Professional Skill Building",
    category_name %in% c("Helping Your Community", "Transportation", "Customer/Human Service", "Healthcare") ~ "Community Service",
    TRUE ~ category_name
  )) %>% 
  select(-new_start_date)

# Set up the application ui ----------------------------------------
ui <- shinyUI(
  navbarPage("MCMF Explorer",
             
             # define the tabs to be used in the app
             # introduction ----------------------------------------
             tabPanel("Introduction",
                      div(style = "height: calc(100% - 40px); position:relative; display: flex; flex-direction: column;",
                          mainPanel(includeMarkdown("intro.Rmd"),
                                    tags$iframe(style = "height: 400px; width: 100%; scrolling=yes",
                                      src="lit-review.pdf")),
                          
                          # create bottom panel
                          div(style = "margin-top: auto; padding: 10px; background-color: #B3DDF2;",
                              h5("Website Links"),
                              p(a("My CHI. My Future (MCMF)", href = "https://explore.mychimyfuture.org")),
                              p(a("MCMF Data", href = "https://data.cityofchicago.org/Events/My-CHI-My-Future-Programs/w22p-bfyb/data")),
                              p(a("CPS ARA", href = "https://www.cps.edu/sites/ara"))
                          )
                      )
             ),
             
             # map ----------------------------------------
             tabPanel("Map",
                      fluidRow(sidebarPanel(width = 3,
                                            h4("Select Data Type:"),
                                            helpText("Choose between the count or proportion of programs in your community"),
                                            radioButtons("data_type",
                                                         label = "",
                                                         choices = c("Count",
                                                                     "Proportion"),
                                                         selected = "Count"),      
                                            h4("Select a Category:"),
                                            helpText("Chose a category to look at its accessibility in your community"),
                                            selectInput("gen_category", 
                                                        label = "", 
                                                        choices = c("Academics",
                                                                    "Leisure & Arts",
                                                                    "Professional Skill Building",
                                                                    "Community Service",
                                                                    "All"),
                                                        selected = "All",
                                                        multiple = FALSE),
                                            h4("Select a Variable:"),
                                            helpText("Choose the variable you want to visualize on the map"),
                                            selectInput("feature",
                                                        label = "",
                                                        choices = c("Number of Programs",
                                                                    "Programs that offer free food")),
                                            h4("Select Grade Level Range:"),
                                            helpText("Select the grade level range you want to see. 0 stands for pre-K and below, and 13 stands for college and above."),
                                            sliderInput("age_range",
                                                        label = "",
                                                        min = 0,
                                                        max = 13,
                                                        value = c(0, 13),
                                                        step = 1)
                      ),
                      mainPanel(plotOutput("map", height = 500)),
                      # add the overlay panel
                      absolutePanel(id = "description",
                                    class = "panel panel-default",
                                    fixed = T,
                                    draggable = T,
                                    top = 90,
                                    left = "auto",
                                    right = 20,
                                    bottom = "auto",
                                    width = "25%",
                                    height = "auto",
                                    # set content of panel
                                    h1("Map of Accessibility"),
                                    p("This map shows the distribution of programs across all the communities of Chicago.",
                                      span(strong("Proportion is only applicable if a specific category is selected"))),
                                    h4("Note the topics included in each category:"),
                                    tags$ul(
                                      tags$li(tags$b("Academics:"), "Academic Support, Math, Reading & Writing, Science, Science & Math, Social Studies, and Teaching"),
                                      tags$li(tags$b("Leisure & Arts:"), "Music & Art, Performance, Sports + Wellness, Nature, and Food"),
                                      tags$li(tags$b("Professional Skill Building:"), "Building & Fixing Things, Computers, Digital Media, Managing Money, Law, and Work + Career"),
                                      tags$li(tags$b("Community Service:"), "Helping Your Community, Transportation, Customer/Human Service, and Healthcare"
                                      ))))),
             # online bar plots ----------------------------------------
             tabPanel("Plots",
                      fluidRow(column(12,
                                      h1("Time Analysis"),
                                      p("We would like to...")
                      )),
                      hr(),
                      fluidRow(sidebarPanel(width = 3,
                                            helpText("Bar plot"),
                                            radioButtons("bar_year", 
                                                         label = "Select a Year:", 
                                                         choices = c("2020",
                                                                     "2021",
                                                                     "2022",
                                                                     "2023"),
                                                         selected = "2020")),
                               mainPanel(plotOutput("bar")))),
             
             # recommendations to stakeholders ----------------------------------------
             tabPanel("Recommendations"
                      # includeMarkdown("intro.Rmd")
                      ),
                      
                      # changing color of menu bar    
                      tags$head(
                        tags$style(
                          HTML(
                            "
          .navbar-default {
            background-color: #B3DDF2 !important;
          }
          "
                          )))
          
          # close the UI definition
             ))
  
  
  
  # Set up the application server ----------------------------------------
  server <- function(input, output) {
    
    # map for map tab panel----------------------------------------  
    output$map <- renderPlot({
      
      fill_var <- switch(input$gen_category,
                         "All" = "sum",
                         "Academics" = "Academics",
                         "Leisure & Arts" = "Leisure & Arts",
                         "Professional Skill Building" = "Professional Skill Building",
                         "Community Service" = "Community Service")
      
      feature <- switch(input$feature,
                        "Number of Programs" = "n_",
                        "Programs that offer free food" = "free_food_")
      
      data_type <- switch(input$data_type,
                          "Count" = "",
                          "Proportion" = "prop_")
      
      # wrangle data
      eda_counts2 <- eda_counts
      gencat_count <- as.data.frame(eda_counts) %>%
        filter(!(min_grade < input$age_range[1] & max_grade < input$age_range[1]) & 
                 !(min_grade > input$age_range[2] & max_grade > input$age_range[2])) %>%
        group_by(community, general_category) %>%
        summarize(n = n(),
                  free_food = sum(program_provides_free_food)) 
      
      gencat_count <- gencat_count %>%
        group_by(community, general_category) %>%
        summarize(n = sum(n),
                  free_food = sum(free_food)) %>% 
        inner_join(as.data.frame(eda_counts2) %>% select(the_geom, community) %>% distinct(), by = "community")
      
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
               `free_food_prop_Professional Skill Building` = replace_na(`free_food_prop_Professional Skill Building`, 0))
      
      # filter based on selected categories
      st_as_sf(gencat_count) %>%
        ggplot() +
        geom_sf(aes(fill = eval(call("$", gencat_count, as.name(paste(feature, data_type, fill_var, sep = "")))))) +
        scale_fill_gradient(name = "Count", low = "white", high = "#FF0000") +
        theme_void()
      
    })
    
    # map for map tab panel---------------------------------------- 
    output$bar <- renderPlot({
      
      # filter based on selected years
      seasons %>% 
        filter(year == input$bar_year) %>% 
        ggplot(mapping = aes(x = program_season, fill = category_name)) + 
        geom_bar() +
        scale_fill_manual(values = c("#FF0000", "#FFFF00", "#B3DDF2","#21B2ED", "#EC4E1B")) +
        labs(x = NULL,
             y = "Count") +
        theme_classic() +
        theme(legend.title= element_blank())
      
    })
    
  }
  
  # run the application  ----------------------------------------
  shinyApp(ui = ui, server = server)

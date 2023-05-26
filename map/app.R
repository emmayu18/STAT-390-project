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
                                                                    "Programs that offer free food"))
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
             tabPanel("Observations",
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

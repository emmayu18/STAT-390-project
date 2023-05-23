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
load("map_counts.rda")


# Set up the application ui ----------------------------------------
ui <- shinyUI(navbarPage("My CHI. My Future. Explorer",
    
    # define the tabs to be used in the app
    # introduction 
    tabPanel("Intro",
             includeMarkdown("intro.Rmd"),
             hr()),
    
    tabPanel("Map",
             fluidRow(column(12,
                             h1("Map of Accessibility to Programs"),
                             p("How accessible are different categories of programs for Chicago's different communities/regions?"),
                             br(),
                             br(),
                             h4("Note the topics included in each category:"),
                             strong("Academics:"), "Academic Support, Math, Reading & Writing, Science, Science & Math, Social Studies, and Teaching",
                             br(),
                             strong("Leisure & Arts:"), "Music & Art, Performance, Sports + Wellness, Nature, and Food",
                             br(),
                             strong("Professional Skill Building:"), "Building & Fixing Things, Computers, Digital Media, Managing Money, Law, and Work + Career",
                             br(),
                             strong("Community Service:"), "Helping Your Community, Transportation, Customer/Human Service, and Healthcare",
             )),
             hr(),
             fluidRow(sidebarPanel(width = 3,
                                   h4("Select a Category:"),
                                   helpText("Chose a category to look at its accessibility in your community"),
                                   radioButtons("gen_category", 
                                                label = "Select a Category:", 
                                                choices = c("Academics",
                                                            "Leisure & Arts",
                                                            "Professional Skill Building",
                                                            "Community Service"),
                                                selected = "Academics")),
                      mainPanel(plotOutput("map", height = 500))
                      ))
    
    # close the UI definition
    ))



# Set up the application server ----------------------------------------
server <- function(input, output) {

    output$map <- renderPlot({
      
      fill_var <- switch(input$gen_category,
                         "Academics" = "Academics",
                         "Leisure & Arts" = "Leisure & Arts",
                         "Professional Skill Building" = "Professional Skill Building",
                         "Community Service" = "Community Service")
      
      # filter based on selected categories
      gencat_count %>%
        filter(general_category == input$gen_category) %>% 
        ggplot() +
        geom_sf(aes(fill = n)) +
        scale_fill_gradient(name = "Count", low = "white", high = "#FF0000") +
        theme_void()
    })
}

# run the application  ----------------------------------------
shinyApp(ui = ui, server = server)

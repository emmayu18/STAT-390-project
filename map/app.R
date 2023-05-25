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
# load("map_counts.rda")
load("fixed_map_counts.rda")



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
                                   selectInput("gen_category", 
                                                label = "Select a Category:", 
                                                choices = c("All",
                                                            "Academics",
                                                            "Leisure & Arts",
                                                            "Professional Skill Building",
                                                            "Community Service"),
                                               selected = "All",
                                               multiple = FALSE),
                                   h4("Select a Variable:"),
                                   helpText("Choose the variable you want to visualize on the map."),
                                   selectInput("feature",
                                               label = "Select a Variable:",
                                               choices = c("Number of Programs",
                                                           "Programs that offer free food")),
                                   h4("Select Data Type:"),
                                   helpText("Choose between count and proportion. Proportion is only applicable if you choose a specific category."),
                                   radioButtons("data_type",
                                                label = "Select a Data Type:",
                                                choices = c("Count",
                                                            "Proportion"),
                                                selected = "Count")),
                      mainPanel(plotOutput("map", height = 500))
                      )),
    
    tabPanel("Online Opportunities")
    
    # close the UI definition
    ))



# Set up the application server ----------------------------------------
server <- function(input, output) {

    output$map <- renderPlot({
      
      # fill_var <- switch(input$gen_category,
      #                    "All" = gencat_count$count_sum,
      #                    "Academics" = gencat_count$n_Academics,
      #                    "Leisure & Arts" = gencat_count$`n_Leisure & Arts`,
      #                    "Professional Skill Building" = gencat_count$`n_Professional Skill Building`,
      #                    "Community Service" = gencat_count$`n_Community Service`)
      
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
}

# run the application  ----------------------------------------
shinyApp(ui = ui, server = server)

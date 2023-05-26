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
                                   helpText("Choose a category to look at its accessibility in your community"),
                                   selectInput("gen_category", 
                                                label = "Select a Category:", 
                                                choices = c("All",
                                                            "Academics",
                                                            "Leisure & Arts",
                                                            "Professional Skill Building",
                                                            "Community Service"),
                                               selected = "All",
                                               multiple = FALSE),
                                   h4("Select an MCMF Variable:"),
                                   helpText("Choose the variable you want to visualize on the My Chi, My Chicago map."),
                                   selectInput("feature",
                                               label = "Select a Variable:",
                                               choices = c("Number of Programs",
                                                           "Programs that offer free food")),
                                   h4("Select a CPS ARA Variable:"),
                                   helpText("Choose the variable you want to visualize on the Chicago Public School Annual Regional Analysis map."),
                                   selectInput("ara_feature",
                                               label = "Select a Variable:",
                                               choices = c("Four-Year Graduation Rate",
                                                           "College Enrollment Rate",
                                                           "Free & Reduced Lunch Rate",
                                                           "No Internet Rate"),
                                               selected = "Four-Year Graduation Rate"),
                                   h4("Select MCMF Fill Data Type:"),
                                   helpText("Choose between count and proportion to display in the fill gradient of the MCMF map. Proportion is only applicable if you choose a specific category."),
                                   radioButtons("data_type",
                                                label = "Select the Fill Data Type:",
                                                choices = c("Count",
                                                            "Proportion"),
                                                selected = "Count"),
                                   h4("Click to Highlight Priority Regions:"),
                                   helpText("Choosing the Highlight option will accentuate the borders of priority regions on both maps"),
                                   radioButtons("priority_highlight",
                                                label = "Click to highlight priority regions:",
                                                choices = c("Remove Highlight", "Highlight"),
                                                selected = "Remove Highlight"),
                                   h4("Select Grade Level Range:"),
                                   helpText("Select the grade level range you want to see. 0 stands for pre-K and below, and 13 stands for college and above."),
                                   sliderInput("age_range",
                                               label = "Select grade range:",
                                               min = 0,
                                               max = 13,
                                               value = c(0, 13),
                                               step = 1)),
                      mainPanel(splitLayout(cellWidths = c("50%", "50%"), 
                                            plotOutput("map1"), 
                                            plotOutput("map2")))
                      )),
    
    tabPanel("Online Opportunities")
    
    # close the UI definition
    ))



# Set up the application server ----------------------------------------
server <- function(input, output) {

    output$map1 <- renderPlot({
      
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
      
      data_type_legend <- switch(input$data_type,
                                 "Count" = "Count",
                                 "Proportion" = "Proportion")
      
      priority_highlight <- switch(input$priority_highlight,
                                   "Remove Highlight" = c(0.1, 0.1),
                                   "Highlight" = c(0.9, 0.1))
      
      # wrangle data
      list_priority_areas = c("Austin", "North Lawndale", "Humboldt Park", 
                              "East Garfield Park", "Englewood", "Auburn Gresham",
                              "West Garfield Park", "Roseland", "Greater Grand Crossing",
                              "West Englewood", "South Shore", "New City", "Chicago Lawn",
                              "South Lawndale", "West Pullman"
      )
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

      # filter based on selected categories
      st_as_sf(gencat_count) %>%
        ggplot() +
        geom_sf(aes(fill = eval(call("$", gencat_count, as.name(paste(feature, data_type, fill_var, sep = ""))))),
                lwd = ifelse(gencat_count$priority == TRUE, priority_highlight[1], priority_highlight[2])) +
        scale_fill_gradient(name = data_type_legend, low = "white", high = "#FF0000") +
        labs(title = "My Chi, My Future") +
        theme_void()
      
    })
    output$map2 <- renderPlot({
      
      priority_highlight <- switch(input$priority_highlight,
                                   "Remove Highlight" = c(0.1, 0.1),
                                   "Highlight" = c(0.9, 0.1))
      
      ara_feature <- switch(input$ara_feature,
                            "Four-Year Graduation Rate" = supp_data$four_year_graduation_rate,
                            "College Enrollment Rate" = supp_data$college_enrollment,
                            "Free & Reduced Lunch Rate" = supp_data$percent_free_reduced_lunch,
                            "No Internet Rate" = supp_data$no_internet)
      
      supp_data %>%
        ggplot() +
        geom_sf(aes(fill = ara_feature),
                lwd = ifelse(supp_data$priority == TRUE, priority_highlight[1], priority_highlight[2])) +
        scale_fill_gradient(name = "Rate", low = "white", high = "#6fbee6") +
        labs(title = "Chicago Public School Annual Regional Analysis") +
        theme_void()
    })
}

# run the application  ----------------------------------------
shinyApp(ui = ui, server = server)

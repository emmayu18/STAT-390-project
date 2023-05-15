# load in packages
library(shiny)
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(gtExtras)
library(formattable)
library(sf)

# load in data
data <- read_tsv("data/convert_MCMF_ALL_TIME_DATA.csv", show_col_types = FALSE) %>% 
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

##### WRANGLING

# data w/ column for length of opportunity
# how should i deal with program_length = 0 ?
time <- data %>%
  mutate(start_date = as.Date(start_date, format = "%m/%d/%y"),
         end_date = parse_date_time(end_date, orders = c("%m/%d/%y")),
         program_length = as.numeric(difftime(end_date, start_date, units = "days")))

# data w/ column for season of each event based on month of start_date
seasons <- data %>%
  mutate(new_start_date = parse_date_time(start_date, orders = c("%m/%d/%y")),
         program_season = case_when(
           month(new_start_date) %in% c(3,4,5) ~ 'spring',
           month(new_start_date) %in% c(6,7,8) ~ 'summer',
           month(new_start_date) %in% c(9,10,11) ~ 'fall',
           month(new_start_date) %in% c(12,1,2) ~ 'winter',
         )) %>% 
  select(-new_start_date)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

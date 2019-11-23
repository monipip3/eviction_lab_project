#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(leaflet)
library(shiny)
library(lubridate)
library(tidyverse)


state_map_data <- read_csv(".//state_map_data")
state_map_data



library(shiny)


 

    ui <- fluidPage(

            sliderInput(inputId = "year",
                        label = "Select a Year:",
                        min = min(state_map_data$year),
                        max = max(state_map_data$year),
                        value = 2010,
                        step = 1),
          
            radioButtons(inputId = "layer",
                        label = "Select a Dataset to View:",
                        choices = c("Eviction Filing Rate", "Percent Rent Burden",
                                       "Percent Renter Occupied", "Poverty Rate")),
            
            selectInput(inputId = "state",
                        label = "Select a State:",
                        choices = unique(eviction_state$name)),
            mainPanel(
                leafletOutput("map"))
            )
                                     
                                  
                                     
    
    # Define server logic required to draw a histogram

    server <- function(input, output, session) {
     
      output$map <- renderLeaflet({   
      leaflet() %>%
        addProviderTiles('Hydda.Full') %>%
        addPolygons(data = state_map_data, fill = state_map_data$poverty_rate)%>%
        setView(lat = 39.8283, lng = -98.5795, zoom = 4)
          
          
        })  
   }

# Run the application 
shinyApp(ui = ui, server = server)

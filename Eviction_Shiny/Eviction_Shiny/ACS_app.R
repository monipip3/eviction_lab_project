#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# load in packages
library(leaflet)
library(shiny)
library(lubridate)
library(tidyverse)
library(shiny)

# load in csv files

#data <- "./data_shiny/data.zip"

#unzip(data, overwrite = FALSE)

# read in csv 
eviction_by_state <- read_csv(".Eviction_Shiny/eviction_by_state.csv")

spatial_state <- as_Spatial(state_us_geo)

state_eviction <- merge(spatial_state, eviction_state_2010, by = "GEOID", duplicateGeoms = TRUE)

purPal <- colorQuantile("PuBuGn", domain = state_eviction$eviction_filing_rate, n = 6)
blugr <- colorQuantile("YlGnBu", domain = state_eviction$pct_renter_occupied, n = 6)


popup_evic <- paste0("<strong>", state_eviction$name,
                     "<strong><br />Median HH Income: <strong>", state_eviction$median_household_income)

map_maker <- function(x){
  if (x == "Eviction Filing Rate") {
    leaflet() %>% 
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = state_eviction, fillColor = ~purPal(state_eviction$eviction_filing_rate), 
                  smoothFactor = 0.2, fillOpacity = .7, weight = 0.2,
                  popup = ~popup_evic) %>%
      addLegend(purPal,
                values = state_eviction$eviction_filing_rate,
                position = "bottomleft",
                title = "Eviction Filing Rates")
  }else{
    leaflet() %>% 
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = state_eviction, 
                  fillColor = ~blugr(state_eviction$pct_renter_occupied),
                  smoothFactor = 0.2, fillOpacity = .7, weight = 0.2,
                  popup = ~popup_evic) %>%
      addLegend(blugr,
                values = state_eviction$pct_renter_occupied,
                position = "bottomleft",
                title = "Percent Renter Occupied") }
 

    ui <- fluidPage(

            selectInput(inputId = "year",
                        label = "Select a Year:",
                        choices = unique(eviction_by_state$year),
                        value = 2012,
                        step = 1),
          
            radioButtons(inputId = "layer",
                        label = "Select a Dataset to View:",
                        choices = c("Eviction Filing Rate", 
                                       "Percent Renter Occupied")),
            selectInput(inputId = "state",
                        label = "Select a State:",
                        choices = unique(eviction_by_state$name)),
            mainPanel(
                leafletOutput("map"))
            )
                                     
    
    # Define server logic required to draw a histogram

    server <- function(input, output, session) {
     
    filtered_data <- reactive({
      state_eviction %>%
        filter(year == input$year)})
      
      x <- reactive({
        input$layer
      })
      
    
    output$map <- renderLeaflet({
      map_maker(x())
      })

}

# Run the application 
shinyApp(ui = ui, server = server)

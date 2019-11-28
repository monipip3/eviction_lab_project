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
library(sf)
library(spatialEco)
library(tigris)
library(sp)

# load in csv files

#data <- "./data_shiny/data.zip"

#unzip(data, overwrite = FALSE)

# read in csv 
eviction_by_state <- read_csv("./eviction_state_2010.csv") 

state_us_geo <- tigris::states(class= "sf")

spatial_state <- as_Spatial(state_us_geo)

state_eviction <- sp::merge(spatial_state, eviction_by_state, by = c("GEOID" = "GEOID"), duplicateGeoms = TRUE)


#state_eviction <- sp.na.omit(state_eviction, margin = 1)


map_maker <- function(x, y){
  purPal <- colorBin("Reds", domain = y$eviction_filing_rate, bins = c(0, 5, 10, 20, 30, 40, 50, 100, 120))
  blugr <- colorBin("YlGnBu", domain = y$pct_renter_occupied, n = 6)
  
  popup_evic <- paste0("<b>","<center>", y$name,
                       "</b>","</center>",
                       "<br />Median HH Income: $", y$median_household_income,
                       "<br />Population: ", y$population,
                       "<br />Percent Rent Burden: ", y$rent_burden,
                       "<br />Eviction Filing Rate: ", y$eviction_filing_rate)
  
  if (x == "Eviction Filing Rate") {
    leaflet() %>% 
      setView(-98.483330, 38.712046, zoom = 3) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = y, fillColor = ~purPal(y$eviction_filing_rate), 
                  smoothFactor = 0.2, fillOpacity = .9, weight = 0.2,
                  popup = ~popup_evic) %>%
      addLegend(purPal,
                values = y$eviction_filing_rate,
                position = "bottomleft",
                title = "Eviction Filing <br/ > Rates")
  }else{
    leaflet() %>% 
      setView(-98.483330, 38.712046, zoom = 3) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data = y, 
                  fillColor = ~blugr(y$pct_renter_occupied),
                  smoothFactor = 0.2, fillOpacity = .9, weight = 0.2,
                  popup = ~popup_evic) %>%
      addLegend(blugr,
                values = y$pct_renter_occupied,
                position = "bottomleft",
                title = "Percent <br/ > Renter Occupied") }}
 

    ui <- fluidPage(
      titlePanel("Eviction Data Mapper"),
        sidebarLayout(
          sidebarPanel(
            radioButtons(inputId = "layer",
                         label = "Select a Dataset to View:",
                         choices = c("Eviction Filing Rate", 
                                     "Percent Renter Occupied")),
            
            selectInput(inputId = "year",
                        label = "Select a Year:",
                        choices = unique(eviction_by_state$year)),
            ),
            mainPanel(
                leafletOutput("map"))
            ))
                                     
    
    # Define server logic required to draw a histogram

    server <- function(input, output, session) {
     
    #filtered_data <- reactive({
      #state_eviction[state_eviction$year == input$year, ]
      
      y <- reactive({
        subset(state_eviction, state_eviction$year == input$year)
      
    })
      
      x <- reactive({
        input$layer
      
    })
        
    output$map <- renderLeaflet({
      
      leaflet() %>% 
        setView(-98.483330, 38.712046, zoom = 4) %>%
        addProviderTiles(providers$CartoDB.Positron)
      
      map_maker(x(), y())
      
      })
}

# Run the application 
shinyApp(ui = ui, server = server)

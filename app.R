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

year <- seq(2000, 2016, 1)

eviction_state


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Analyzing Eviction Filing Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        #sidebarPanel(
            sliderInput(inputId = "years",
                        label = "Select a Year:",
                        min = 2000,
                        max = 2016,
                        value = 2010,
                        step = year),
            selectInput(inputId = "state",
                        label = "Select a State:",
                        choices = unique(eviction_state$name),
            radioButtons(inputId = "layer",
                         label = "Select a Dataset to View:",
                         choices = c("Eviction Filing Rate", "Poverty Rate", "Percent Renter Occupied", 
                                     "Rent Burden"),
                        

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("mapPlot")
        )
    )
)))

# Define server logic required to draw a histogram

qpal <- colorQuantile("Reds", eviction_state$eviction_filing_rate, n = 6)
server <- function(input, output) {

    output[[mapPlot]] <- renderLeaflet({
        addPolygons(data = eviction_state, fillColor = ~qpal(eviction_state$eviction_filing_rate)
      
    }))
}

# Run the application 
shinyApp(ui = ui, server = server)

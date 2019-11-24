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

#read in data 
#spatial_data <- "./data_shiny/data.zip"
#unzip(spatial_data, overwrite = FALSE)
#
eviction_county_2010 <- read_csv("./eviction_county_2010.csv")
eviction_by_state <- read_csv("./eviction_by_state.csv")


ui <- fluidPage(
  sliderInput(inputId = "year",
              label = "Select a Year:",
              min = 2010,
              #min = min(as.numeric(eviction_county_2010$year)),
              #max = max(as.numeric(eviction_county_2010$year)),
              max = 2016,
              value = 2010,
              step = 1),
  
  selectInput(inputId = "state",
              label = "Select a State:",
              eviction_county_2010$parent_location),
  #choices = unique(eviction_county_2010$parent_location)),
  selectInput(inputId = "county",
              label = "Select a County:",
              choices = NULL),
  selectInput(inputId = "indp_vars_type",
              label = "Select a Variable Type:",
              choices = c("Demographics","Other")),
  selectInput('ycol', 'Y Variable', names(eviction_county_2010),selected=names(eviction_county_2010)[[1]]),
  #selectInput('ycol', 'Y Variable', names(eviction_county_2010),
  #selected=names(eviction_county_2010)[[2]]),
  actionButton(inputId = "graph", "Create a Trendline"),
  mainPanel(
    plotOutput(outputId='county_trendlines'),
    tableOutput('table'))
)




server <- function(input, output, session) {
  # selectData <- reactive({
  #   as.data.frame(filter(eviction_county_2010,name==input$county) %>%
  #   select(input$xcol,year),col_names=TRUE)
  # })
  
  
  observe({
    x <- filter(eviction_county_2010,parent_location == input$state) %>%
      select(name)
    updateSelectInput(session,"county","Select a County:",choices = unique(x))}
  )
  
  data <- eventReactive(input$graph,{
    filter(eviction_county_2010,name==input$county & parent_location == input$state) %>%
      select(input$ycol,year)
  }) 
  output$table <- renderTable(
    data()
  )
  
   output$county_trendlines <- renderPlot({
     ggplot(data = data(),aes_string(x='year',y=input$ycol)) +
       geom_line()})
} 

# Run the application 
shinyApp(ui = ui, server = server)

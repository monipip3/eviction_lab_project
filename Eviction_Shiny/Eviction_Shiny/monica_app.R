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
library(purrr)

#read in data 
#spatial_data <- "./data_shiny/data.zip"
#unzip(spatial_data, overwrite = FALSE)
#
eviction_county_2010 <- read_csv("./eviction_county_2010.csv")
eviction_by_state <- read_csv("./eviction_by_state.csv")

eviction_county_2010$county_state <- str_c(tools::toTitleCase(as.character(eviction_county_2010$name)), 
                                           eviction_county_2010$parent_location, sep = ", ")

ui <- fluidPage(
  
  selectInput(inputId = "state",
              label = "Select a State:",
              eviction_county_2010$parent_location),
  #choices = unique(eviction_county_2010$parent_location)),
  selectInput(inputId = "county",
              label = "Select a County:",
              choices = NULL),
  selectInput('ycol', 'Y Variable', 
              choices = c('eviction_filing_rate','unemployment_rate')),
  actionButton(inputId = "graph", "Create a Trendline"),
  mainPanel(
    plotOutput(outputId='county_trendlines'),
    tableOutput('similar_counties'))
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
  # output$table <- renderTable(
  #   data()
  # )
  
  output$county_trendlines <- renderPlot({
    ggplot(data = data(),aes_string(x='year',y=input$ycol)) +
      geom_line(aes(color=input$ycol)) +
      theme_minimal() +
      ggtitle(paste0(input$ycol," trended by year"))+
      scale_fill_brewer(palette = "Spectral")
  })
  
  z <- reactive({filter(eviction_county_2010,parent_location == input$state & name == input$county)%>%
      dplyr::select(cluster) %>%unique() %>% pull()})
  
  output$similar_counties <- #eventReactive(input$graph,{
    renderTable({
      eviction_county_2010 %>%
        group_by(county_state) %>%
        filter(cluster==z()) %>%
        summarise_at(c("eviction_filing_rate","population", "cluster", "poverty_rate", "unemployment_rate", "pct_renter_occupied",
                       "Percent_Rural", "median_gross_rent", "median_household_income",
                       "median_property_value", "rent_burden", "pct_white", "pct_nonwhite"), mean, na.rm = TRUE) %>%
        rename(`Eviction Filing Rate`=eviction_filing_rate,Population = population, `Poverty Rate` = poverty_rate,
               `Unemployment Rate` = unemployment_rate, `% Renter Occupied` = pct_renter_occupied,
               `% Rural` = Percent_Rural, `Median Gross Rent` = median_gross_rent, `Median Household Income` = median_household_income,
               `Median Property Value` = median_property_value, `Rent Burden` = rent_burden,
               `% White` = pct_white, `% Non White` = pct_nonwhite) %>% sample_n(4) %>%
        rbind(
          filter(eviction_county_2010,name==input$county & parent_location == input$state) %>%
            group_by(county_state) %>%
            summarise_at(c("eviction_filing_rate","population", "cluster", "poverty_rate", "unemployment_rate", "pct_renter_occupied",
                           "Percent_Rural", "median_gross_rent", "median_household_income",
                           "median_property_value", "rent_burden", "pct_white", "pct_nonwhite"), mean, na.rm = TRUE) %>%
            rename(`Eviction Filing Rate`=eviction_filing_rate,Population = population, `Poverty Rate` = poverty_rate,
                   `Unemployment Rate` = unemployment_rate, `% Renter Occupied` = pct_renter_occupied,
                   `% Rural` = Percent_Rural, `Median Gross Rent` = median_gross_rent, `Median Household Income` = median_household_income,
                   `Median Property Value` = median_property_value, `Rent Burden` = rent_burden,
                   `% White` = pct_white, `% Non White` = pct_nonwhite) %>%
            lapply(FUN=as.numeric) %>%
            lapply(FUN =round)
        )
    })
  #})
} 

# Run the application 
shinyApp(ui = ui, server = server)
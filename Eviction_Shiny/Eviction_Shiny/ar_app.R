#
# This application code deals with creating summary tables for comparisons of similar counties
# filtering by variables and county. This will include code to select similar counties from
# the same cluster.
#

library(shiny)
library(tidyverse)
library(datasets)
library(lubridate)
library(stringr)

# zip <- "./data_shiny/data.zip"
# unzip(zip, overwrite = FALSE)
eviction_county_2010 <- read.csv("./eviction_county_2010.csv")
eviction_by_state <- read_csv("./eviction_by_state.csv")

eviction_county_2010$County <- str_c(tools::toTitleCase(as.character(eviction_county_2010$name)),
                                               eviction_county_2010$parent_location, sep = ", ")

# ec2010_means <- eviction_county_2010 %>%
#   group_by(GEOID) %>%
#   mutate(Population = mean(population, na.rm = TRUE),
#          `Poverty Rate` = mean(poverty_rate, na.rm = TRUE),
#          `% Renter Occupied` = mean(pct_renter_occupied, na.rm = TRUE),
#          `Median Gross Rent` = mean(median_gross_rent, na.rm = TRUE),
#          `Median Household Income` = mean(median_household_income, na.rm = TRUE),
#          `Median Property Value` = mean(median_property_value, na.rm = TRUE),
#          `Rent Burden` = mean(rent_burden, na.rm = TRUE),
#          `% White` = mean(pct_white, na.rm = TRUE),
#          `% African American` = mean(pct_af_am, na.rm = TRUE),
#          `% Hispanic` = mean(pct_hispanic, na.rm = TRUE),
#          `% American Indian` = mean(pct_am_ind, na.rm = TRUE),
#          `% Asian` = mean(pct_asian, na.rm = TRUE),
#          `% Native Hawaiian/Pacific Islander` = mean(pct_nh_pi, na.rm = TRUE),
#          `% Multiple` = mean(pct_multiple, na.rm = TRUE),
#          `% Other` = mean(pct_other, na.rm = TRUE),
#          `% Non White` = mean(pct_nonwhite, na.rm = TRUE),
#          `% Rural` = mean(Percent_Rural, na.rm = TRUE),
#          `Unemployment Rate` = mean(unemployment_rate, na.rm = TRUE))
# ec2010_means <- subset(ec2010_means, select=c("GEOID", "County", "Population",
#                                               "Poverty Rate", "Unemployment Rate", "% Renter Occupied",
#                                               "% Rural", "Median Gross Rent", "Median Household Income",
#                                               "Median Property Value", "Rent Burden", "% White", "% Non White",
#                                               "% African American", "% Hispanic", "% American Indian", "% Asian",
#                                               "% Native Hawaiian/Pacific Islander", "% Multiple", "% Other",
#                                               "parent_location", "name"))
# ec2010_means <- ec2010_means %>%
#   distinct()
# ec2010_means_transpose <- as.data.frame(t(as.matrix(ec2010_means)))
# colnames(ec2010_means_transpose) <- unlist(ec2010_means_transpose[row.names(ec2010_means_transpose)=='County',])
# ec2010_means_transpose <- ec2010_means_transpose[!row.names(ec2010_means_transpose)=='County',]

ui <- fluidPage(
  sliderInput(inputId = "year",
              label = "Select a Year:",
              min = 2010,
              max = 2016,
              value = 2010,
              step = 1),

  radioButtons(inputId = "layer",
               label = "Select a Dataset to View:",
               choices = c("Eviction Filing Rate"="eviction_filing_rate", "Percent Rent Burden"="rent_burden",
                           "Percent Renter Occupied"="pct_renter_occupied", "Poverty Rate"="poverty_rate")),

  selectInput(inputId = "state",
              label = "Select a State:",
              eviction_county_2010$parent_location),
  selectInput(inputId = "county",
              label = "Select a County:",
              choices = NULL),
  mainPanel(
    h2("Comparisons Across Similar Counties"),
    tableOutput('table')
  )
)

server <- function(input, output, session) {

  observe({
    x <- filter(eviction_county_2010,parent_location == input$state) %>%
      select(name)
    updateSelectInput(session,"county","Select a County:",choices = unique(x))}
  )

  # ec <- reactive({
    # ec <- eviction_county_2010 %>%
    #   filter(parent_location == input$state) %>%
    #   filter(name == input$county)
    # sel_clust <- c(unique(ec$cluster))
    # sel_geoid <- c(unique(ec$GEOID))
    # sim_cty <- eviction_county_2010 %>% filter(cluster == sel_clust | GEOID != sel_geoid)
  #   # sim_cty <- unique(simil_ctys$GEOID)
  #   # sim_cty <- sample(simil_ctys, 5)
  #   # sim_cty <- append(sel_geoid, simil_ctys)

  z <- reactive({filter(eviction_county_2010,parent_location == input$state & name == input$county)%>%
    select(cluster) %>%unique() %>% pull()})



  output$table <- renderTable(
    eviction_county_2010 %>%
      group_by(County) %>%
      filter(cluster==z()) %>%
      summarise_at(c("GEOID", "population", "cluster", "poverty_rate", "unemployment_rate", "pct_renter_occupied",
                     "Percent_Rural", "median_gross_rent", "median_household_income",
                     "median_property_value", "rent_burden", "pct_white", "pct_nonwhite",
                     "pct_af_am", "pct_hispanic", "pct_am_ind", "pct_asian",
                     "pct_nh_pi", "pct_multiple", "pct_other"), mean, na.rm = TRUE) %>%
      rename(Population = population, `Poverty Rate` = poverty_rate,
             `Unemployment Rate` = unemployment_rate, `% Renter Occupied` = pct_renter_occupied,
             `% Rural` = Percent_Rural, `Median Gross Rent` = median_gross_rent, `Median Household Income` = median_household_income,
             `Median Property Value` = median_property_value, `Rent Burden` = rent_burden,
             `% White` = pct_white, `% Non White` = pct_nonwhite,
             `% African American` = pct_af_am, `% Hispanic` = pct_hispanic, `% American Indian` = pct_am_ind,
             `% Asian` = pct_asian,
             `% Native Hawaiian/Pacific Islander` = pct_nh_pi, `% Multiple` = pct_multiple, `% Other` = pct_other) # %>%
      #filter(cluster==y)
      #filter(County == str_c(input$county, input$state, sep = ", "))
  )
}

# Run the application
shinyApp(ui = ui, server = server)

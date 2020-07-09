#
# This is the user-interface definition of a Shiny web application. 

library(shiny)
library(shinythemes)
library(shinycssloaders)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = shinytheme("yeti"),

    # Application title
    titlePanel("Air pollution in the Czech Republic (2013-2018)"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            radioButtons(inputId = "pollutant", 
                         label = strong("Pollutants"),
                         choices = list("PM2.5", "PM10", "SO2", "NO2"),
                         selected = "PM2.5"
            ),
            hr(),
            selectizeInput(inputId = "stationsList", 
                           label = strong("Stations (select up to 4)"), 
                           stations_data$StationName,
                           selected = stations_data$StationName[1],
                           multiple=TRUE, 
                           options = list(maxItems = 4)
            ),
            hr(),
            dateRangeInput(inputId = "dateRange",
                           label = strong("Date range (yyyy-mm-dd)"),
                           min = "2013-01-01",
                           max = "2018-12-31",
                           start = "2013-01-01",
                           end = "2018-12-31",
                           startview = "year",
                           separator = " to "
            ),
            hr(),
            sliderInput(inputId = "hour", 
                        label = strong("Hour in the day"), 
                        min = 0, max = 24, value = c(0, 24),
                        step = 1
            ),
            hr(),
            radioButtons(inputId = "selectAggregation", 
                         label = strong("Aggregation"),
                         choices = list("Raw hourly data", 
                                        "Daily average", 
                                        "Daily maxima", 
                                        "Hours per day for which threshold exceeded",
                                        "Hours per year for which threshold exceeded",
                                        "Days per year for which daily average concentration exceeds threshold"),
                         selected = "Raw hourly data"
            ),
            conditionalPanel(
                condition = "input.selectAggregation == 'Hours per day for which threshold exceeded' 
                            || input.selectAggregation == 'Hours per year for which threshold exceeded'                                        
                            || input.selectAggregation ==  'Days per year for which daily average concentration exceeds threshold'
                            ",
                             sliderInput(inputId = "threshold", 
                                         label = strong("Threshold"), 
                                         min = 50, max = 300, value = 200,
                                         step = 10
                            )
            )
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Plot",
                    withSpinner(plotOutput("distPlot")),
                    downloadButton("report", "Download plot and table")
                ),
                tabPanel("Map",
                    withSpinner(plotOutput("map"))
                ),
                tabPanel("Table",
                    withSpinner(DT::DTOutput("table")),
                    hr(),
                    downloadButton("downloadDataset", "Download Dataset")
                )
            )
        )
    )
))

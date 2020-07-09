#
# This is the server logic of a Shiny web application.
#

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    library(shiny)
    library(tidyverse)
    library(tidyr)
    library(dplyr)
    library(lubridate)
    library(ggplot2)
    
    #load Stations
    stations_data <- read.csv("Project/__Stations.csv", stringsAsFactors = FALSE)
    
    stations_data <- stations_data %>%
        select(everything(), -Network) %>%
        arrange(StationName)
    
    #load pollution data based on pollutant code
    folder <- "/Users/paulina/_Glasgow/R/AirPollution_Project/Project/"
    pm25_list <- list.files(path = folder, pattern = "PM2")
    pm10_list <- list.files(path = folder, pattern = "PM10")
    so2_list <- list.files(path = folder, pattern = "SO2")
    no2_list <- list.files(path = folder, pattern = "NO2")
    all_list <- list.files(path = folder, pattern = "CZ")
    
    pm25_data <- do.call("rbind", lapply(pm25_list, 
                                         function(x) read.csv(paste(folder, x, sep=''), 
                                                              stringsAsFactors = FALSE)))
    pm10_data <- do.call("rbind", lapply(pm10_list, 
                                         function(x) read.csv(paste(folder, x, sep=''), 
                                                              stringsAsFactors = FALSE)))
    so2_data <- do.call("rbind", lapply(so2_list, 
                                        function(x) read.csv(paste(folder, x, sep=''), 
                                                             stringsAsFactors = FALSE)))
    no2_data <- do.call("rbind", lapply(no2_list, 
                                        function(x) read.csv(paste(folder, x, sep=''), 
                                                             stringsAsFactors = FALSE)))
    
    #tidying data
    
    #getting data for the plot
    
    pollutants <- reactive({input$pollutant})
    stationsList <- reactive({input$stationsList})
    aggregation <- reactive({input$selectAggregation})
    hour <- reactive({input$hour})
    threshold <- reactive({input$threshold})
    
    data <- reactive({
        # select data set based on input$.. from ui.R
        
        if (pollutants() == "PM2.5") {
            data <- pm25_data %>%
                mutate(Date = make_datetime(Year, Month, Day, Hour)) %>%
                select(Date, everything()) %>%
                select(-(Year:Day))
        } else if (pollutants() == "PM10") {
            data <- pm10_data %>%
                mutate(Date = make_datetime(Year, Month, Day, Hour)) %>%
                select(Date, everything()) %>%
                select(-(Year:Day))
        } else if (pollutants() == "SO2") {
            data <- no2_data %>%
                mutate(Date = make_datetime(Year, Month, Day, Hour)) %>%
                select(Date, everything()) %>%
                select(-(Year:Day))
        } else {
            data <- so2_data %>%
                mutate(Date = make_datetime(Year, Month, Day, Hour)) %>%
                select(Date, everything()) %>%
                select(-(Year:Day))
        }
    })
    
    rawHourlyData <- reactive({
        # joining tables
        mergedData <- merge(x = data(), y = stations_data, by.x = c("AirQualityStationEoICode"), by.y = c("EoICode"), all.y=TRUE)
        mergedData %>%
            filter(StationName %in% stationsList()) %>%
            filter(Date >= input$dateRange[1] & Date <= input$dateRange[2]) %>%
            filter(Hour >= input$hour[1] & Hour <= input$hour[2])
    })
    
    dailyAverages <- reactive({
        rawHourlyData() %>%
            group_by(Date, AirPollutant, AirQualityStationEoICode, StationName, Latitude, Longitude) %>%
            summarise(averageDailyConcentration = mean(Concentration))
    })
    
    dailyMaxima <- reactive({
        rawHourlyData() %>%
            group_by(Date, AirPollutant, AirQualityStationEoICode, StationName, Latitude, Longitude) %>%
            summarise(maxDailyConcentration = max(Concentration))
    })
    
    thresholdData <- reactive({
            if (aggregation() == "Hours per day for which threshold exceeded") {
                rawHourlyData() %>%
                    mutate(Difference = Concentration - threshold()) %>%
                    mutate(ExceedsThreshold = ifelse(Difference >=0, 1, 0)) %>%
                    group_by(Date, AirPollutant, AirQualityStationEoICode, StationName, Latitude, Longitude) %>%
                    summarise(HoursCount = sum(ExceedsThreshold))
            }  else if (aggregation() == "Hours per year for which threshold exceeded") {
                rawHourlyData() %>%
                    mutate(Difference = Concentration - threshold()) %>%
                    mutate(ExceedsThreshold = ifelse(Difference >=0, 1, 0)) %>%
                    group_by(Date, AirPollutant, AirQualityStationEoICode, StationName, Latitude, Longitude) %>%
                    summarise(HoursCount = sum(ExceedsThreshold))
            } else {
                rawHourlyData() %>%
                    mutate(Difference = Concentration - threshold()) %>%
                    mutate(ExceedsThreshold = ifelse(Difference >=0, 1, 0)) %>%
                    group_by(day(Date), AirPollutant, AirQualityStationEoICode, StationName, Latitude, Longitude) %>%
                    summarise(HoursCount = sum(ExceedsThreshold))
            }
    })
    
    useData <- reactive({
        if (aggregation() == "Raw hourly data") {
            rawHourlyData()
        } else if (aggregation() == "Daily average") {
            dailyAverages()
        } else if (aggregation() == "Daily maxima") {
            dailyMaxima()
        } else if (aggregation() == "Hours per day for which threshold exceeded") {
            #do nothing yet
        }  else if (aggregation() == "Hours per year for which threshold exceeded") {
            #do nothing yet
        } else {
            
        }
    })
    
    dates <- reactive({
        useData() %>%
            summarise(min = min(useData()$Date, na.rm = TRUE),
                      max = max(useData()$Date, na.rm = TRUE)
                      )
    })
    
    observe({
        updateDateRangeInput(session, "dateRange",
        start = dates()[1], 
        end = dates()[2], 
        min = dates()[1],
        max = dates()[2]
        )
    })
    
    output$distPlot <- renderPlot({
        # draw the time series plot
        p <- ggplot(useData())
        if (aggregation() == "Raw hourly data") {
           p <- p + aes(x = Date, y = Concentration, color = StationName) +
                geom_line() +
               labs(color = "Station Name")
           if (pollutants() == "SO2") {
                p <- p + geom_hline(aes(yintercept = 350, linetype = "EU Air Quality Standard"), colour= "blue")  
           } else if (pollutants() == "NO2") {
               p <- p + geom_hline(aes(yintercept = 200, linetype = "EU Air Quality Standard"), colour= "blue")  
           } else {
                #do nothing
            }
                
        } else if (aggregation() == "Daily average") {
            p <- p + aes(x = Date, y = averageDailyConcentration, color = StationName) +
                geom_line() +
                labs(y = "Average Daily Concentration", color = "Station Name")
            if (pollutants() == "PM10") {
                p <- p + geom_hline(aes(yintercept = 50, linetype = "EU Air Quality Standard"), colour= "blue")  
            } else if (pollutants() == "SO2") {
                p <- p + geom_hline(aes(yintercept = 125, linetype = "EU Air Quality Standard"), colour= "blue")  
            } else {
                #do nothing
            }
                
        } else if (aggregation() == "Daily maxima") {
            p <- p + aes(x = Date, y = maxDailyConcentration, color = StationName) +
                geom_line() +
                labs(y = "Maximum Daily Concentration", color = "Station Name")
            if (pollutants() == "PM10") {
                p <- p + geom_hline(aes(yintercept = 50, linetype = "EU Air Quality Standard"), colour= "blue")  
            } else if (pollutants() == "SO2") {
                p <- p + geom_hline(aes(yintercept = 125, linetype = "EU Air Quality Standard"), colour= "blue")  
            } else {
                #do nothing
            }
                
        } else {
            #do nothing yet
        }
        p + labs(caption = "(based on data from European Environmental Agency)")
    })
    
    output$map <- renderPlot({
        maps::map("world", "CZech Republic")
        points(useData()$Longitude, useData()$Latitude, pch = 16, col = "blue")
        text(useData()$Longitude, y = useData()$Latitude, useData()$StationName, pos = 4)
    })
    
    output$table <- DT::renderDT({
        useData()
    })
    
    output$downloadDataset <- downloadHandler(
        filename = "airPollutionPlotData.csv",
        content = function(file) {
            write.csv(useData(), file, row.names = FALSE)
        }
    )
    
    output$report <- downloadHandler(
        filename = "report.doc",
        content = function(file) {
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            
            params <- list(n = input$slider)
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
})

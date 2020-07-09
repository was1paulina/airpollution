library(tidyr)
library(dplyr)

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
allPollutionData <- do.call("rbind", lapply(all_list, 
                                            function(x) read.csv(paste(folder, x, sep=''),
                                                                 stringsAsFactors = FALSE)))

#tidying data
library(lubridate)
pm25_data <- pm25_data %>%
  mutate(DateTime = make_datetime(Year, Month, Day, Hour)) %>%
  select(DateTime, everything()) %>%
  select(-(Year:Hour))

pm10_data <- pm10_data %>%
  mutate(DateTime = make_datetime(Year, Month, Day, Hour)) %>%
  select(DateTime, everything()) %>%
  select(-(Year:Hour))

no2_data <- no2_data %>%
  mutate(DateTime = make_datetime(Year, Month, Day, Hour)) %>%
  select(DateTime, everything()) %>%
  select(-(Year:Hour))

so2_data <- so2_data %>%
  mutate(DateTime = make_datetime(Year, Month, Day, Hour)) %>%
  select(DateTime, everything()) %>%
  select(-(Year:Hour))

pollutions <- allPollutionData %>%
  mutate(DateTime = make_datetime(Year, Month, Day, Hour)) %>%
  select(DateTime, everything()) %>%
  select(-(Year:Hour))

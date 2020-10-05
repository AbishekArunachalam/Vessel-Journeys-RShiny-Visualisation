library(dplyr)
library(shiny)
library(shinyWidgets)
library(leaflet)

# setwd('~/Documents/AppsilonDataScienceTask/')
options(shiny.maxRequestSize=550*1024^2)

ships <- read.csv('./ships.csv')
dim(ships) # 3,102,887 rows, 26 columns
unique(ships$ship_type) # unique ship types

ships <- ships %>% 
  filter(ship_type != 'Unspecified') # remove unspecified ship_type category
dim(ships) # 2,996,023 rows

ship_type <- c('Cargo', 'Tanker', 'Tug', 'Fishing', 'Passenger', 'Pleasure', 'Navigation', 'High Special')
names(ship_type) <- c('Cargo', 'Tanker', 'Tug', 'Fishing', 'Passenger', 'Pleasure', 'Navigation', 'High Special')

calculate_max_distance <- function(vessel_type, vessel_name){
  ships <- ships %>%
    filter(ship_type == vessel_type & SHIPNAME == vessel_name)
  num_rows <- nrow(ships)
  max_distance_covered = 0
  out = list()
  time_taken_buffer = 0
  for(row in 1:(num_rows-1)){
    start_time <- strptime(ships[row, 'DATETIME'], format = "%Y-%m-%d %H:%M:%S", tz="Poland")
    end_time <- strptime(ships[row+1, 'DATETIME'], format = "%Y-%m-%d %H:%M:%S", tz="Poland")
    speed = (ships[row, "SPEED"]) # convert speed from m/hr to m/sec
    time_taken <- as.numeric(difftime(end_time, start_time, units = "secs")) # calculate time difference in seconds
    distance <- (speed * time_taken) # calculate distance in meters
    Sys.sleep(0.001)
    if(distance >= max_distance_covered){
      if(row == (num_rows-2) & length(out) == 0){
        source <- ships[row, c('LON','LAT')]
        destination <- ships[row+1, c('LON','LAT')]
        journey <- rbind(source, destination)
        colnames(journey) = c('long', 'lat')
        out$journey <- journey
        out$max_distance_covered <- distance
      }
      if(distance == max_distance_covered & (time_taken > time_taken_buffer)){
        next
      }
      source <- ships[row, c('LON','LAT')]
      destination <- ships[row+1, c('LON','LAT')]
      journey <- rbind(source, destination)
      colnames(journey) = c('long', 'lat')
      time_taken_buffer = time_taken
      out$journey <- journey
      out$max_distance_covered <- distance
    }
  }
  return(out)
}
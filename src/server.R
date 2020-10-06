library(dplyr)
library(shiny)
library(shinyWidgets)
library(leaflet)

#setwd('~/Documents/Appsilon/')
options(shiny.maxRequestSize=550*1024^2)
options(warn = -1)

ships <- read.csv('./ships.csv')
dim(ships) # 3,102,887 rows, 26 columns
unique(ships$ship_type) # unique ship types

ships <- ships %>% 
  filter(ship_type != 'Unspecified') # remove unspecified ship_type category
dim(ships) # 2,996,023 rows

ship_type <- c('Cargo', 'Tanker', 'Tug', 'Fishing', 'Passenger', 'Pleasure', 'Navigation', 'High Special')
names(ship_type) <- c('Cargo', 'Tanker', 'Tug', 'Fishing', 'Passenger', 'Pleasure', 'Navigation', 'High Special')

findJourney <- function(vessel_type, vessel_name){
  ############################################
  # calcuate max distance covered by each vessel 
  # and the source and destination of journey
  
  # param:
  # vessel_type - type of vessel
  # vessel_name - name of vessel
  # return:
  # max distance covered and the journey dataframe
  ############################################
  ships <- ships %>%
    filter(ship_type == vessel_type & SHIPNAME == vessel_name)
  
  num_rows <- nrow(ships)
  max_distance_covered = 0
  time_taken_buffer = 0
  output = list()
  
  for(row in 1:(num_rows-1)){
    start_time <- strptime(ships[row, 'DATETIME'], format = "%Y-%m-%d %H:%M:%S", tz="Poland")
    end_time <- strptime(ships[row+1, 'DATETIME'], format = "%Y-%m-%d %H:%M:%S", tz="Poland")
    speed = (ships[row, "SPEED"]) # convert speed from m/hr to m/sec
    time_taken <- as.numeric(difftime(end_time, start_time, units = "secs")) # calculate time difference in seconds
    distance <- (speed * time_taken) # calculate distance in meters
    
    if(distance >= max_distance_covered){
      # case: if distances are equal and time taken is more skip loop
      if(distance == max_distance_covered & (time_taken > time_taken_buffer)){
        next
      }
      source <- ships[row, c('LON','LAT')]
      destination <- ships[row+1, c('LON','LAT')]
      journey <- rbind(source, destination)
      colnames(journey) = c('long', 'lat')
      time_taken_buffer = time_taken
      output$journey <- journey
      output$max_distance_covered <- distance
    }
  }
  return(output)
}

server <- function(input, output) {
  
  output$ship_type <- renderUI({
    selectInput("vessel_type_dropdown", "", 
                choices = ship_type,
                selected = "High Special")
  })
  
  output$ship_name <- renderUI({
    req(input$vessel_type_dropdown)
    selectInput("vessel_name_dropdown", "", 
                choices = unique(ships[ships$ship_type == input$vessel_type_dropdown,'SHIPNAME']),
                selected = "VALO")
  })
  
  max_journey <- reactive({
    req(input$vessel_type_dropdown, input$vessel_name_dropdown)
    max_journey <- findJourney(input$vessel_type_dropdown, input$vessel_name_dropdown)
    max_journey
  })
  
  output$distance_covered <- renderText({
    req(input$vessel_type_dropdown, max_journey()$max_distance_covered)
    paste0("The distance covered by ", input$vessel_name_dropdown, " is ", 
           toString(max_journey()$max_distance_covered), " meters.")
  })
  
  output$mymap <- renderLeaflet({
    req(max_journey()$journey)
    leaflet(data = max_journey()$journey) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels, group = "Carto DB dark") %>%
      addCircleMarkers(lng = ~long, 
                       lat = ~lat, 
                       radius = 5, 
                       fillOpacity = 0.5,
                       weight = 2,
                       opacity = 0.8, 
                       color = "red") %>%
      addPolylines(lat = ~lat, 
                   lng = ~long, 
                   weight = 1, 
                   opacity = 0.5, 
                   color = "#820a0a") %>%
      addLayersControl(baseGroups = c("Carto DB Positron","Carto DB dark"))
  })
}


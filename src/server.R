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


server <- function(input, output) {
  
  #Sys.sleep(0.2)
  output$ship_type <- renderUI({
    selectInput("vessel_type_dropdown", "", 
                choices = ship_type,
                selected = "High Special")
  })
  output$ship_name <- renderUI({
    #req(input$vessel_type_dropdown)
    validate(need(length(input$vessel_type_dropdown)>0, ""))
    selectInput("vessel_name_dropdown", "", 
                choices = unique(ships[ships$ship_type == input$vessel_type_dropdown,'SHIPNAME']),
                selected = "VALO")
  })
  
  max_journey <- reactive({
    req(input$vessel_type_dropdown, input$vessel_name_dropdown)
    #validate(need(length(input$vessel_type_dropdown)>0, ""))
    #validate(need(length(input$vessel_name_dropdown)>0, ""))
    max_journey <- calculate_max_distance(input$vessel_type_dropdown, input$vessel_name_dropdown)
    max_journey
  })
  
  output$distance_covered <- renderText({
    # validate(need(length(input$vessel_type_dropdown)>0, ""))
    # validate(need(!is.na(max_journey()$max_distance_covered)), "")
    #req(input$vessel_type_dropdown, max_journey()$max_distance_covered)
    if(is.null(max_journey()))return()
    paste0("The distance covered by ", input$vessel_name_dropdown, " is ", toString(max_journey()$max_distance_covered), " meters.")
    
  })
  
  output$mymap <- renderLeaflet({
    if(is.null(max_journey()$journey))return()
    #req(max_journey()$journey)
    #validate(need(!is.na(max_journey()$journey)), "")
    #if(!is.na(max_journey()$journey)){
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

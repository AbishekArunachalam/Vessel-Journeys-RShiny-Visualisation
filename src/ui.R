library(leaflet)

navbarPage(h5("Appsilon Data Science"),
           tabPanel(
             h6("Vessel Journey Visualisation"),
             div(class="outer",
                 tags$head(
                   # Include custom CSS
                  includeCSS("styles.css")), 
                 leafletOutput("mymap", width="100%", height="100%"),
                 absolutePanel(id = "controls", 
                               class = "panel panel-default", 
                               fixed = TRUE,
                               draggable = TRUE, 
                               top = 100, 
                               right = 20, 
                               left = "auto", 
                               bottom = "auto",
                               width = 290,
                               height = "auto",
                               fluidRow(div(h5("Vessel type:"),
                                            uiOutput("ship_type"))),
                               fluidRow(div(h5("Vessel name:"),
                                            uiOutput("ship_name"))),
                               br(),
                               fluidRow(div(textOutput("distance_covered")))
                               )
                 )
             )
           )
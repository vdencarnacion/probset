library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(tidyverse)
library(leaflet)
library(geojsonR)
library(rgdal)
library(dplyr)

library(jsonlite)
library(readr)
source('preprocess.R')
postcodes <- readOGR("./au-postcodes-Visvalingam-0.1.geojson",
                          layer="au-postcodes-Visvalingam-0.1")
# countries <- readOGR("./accor_countries.geojson", layer="accor_countries")
df_main <- preprocess_au_service_list('./Australia-30-June-2019-v2-1.csv')
  
ui <- fluidPage(
   
   # Application title
   titlePanel("Australian Aged Care Units"),
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Number of bins:",
                     min = 1,
                     max = 50,
                     value = 30),
         
         actionButton(inputId = "go",
                      label = "Go"),
         
         width = 2
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Australian Map",
                   leafletOutput(outputId = "world_map")
          ),
          id = "tabs"
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # AUSTRALIAN MAP
  output$world_map <- renderLeaflet({
    leaflet(postcodes,
            # countries,
            options = leafletOptions(dragging = TRUE,
                                     minZoom = 2)
            ) %>%
      
      # leaflet(countries) %>%
      addTiles() %>%
      addPolygons(weight=1, smoothFactor = 0.2, fillOpacity = 0) %>%
      # clearMarkers()  %>%
      setView(lng = 133.583, lat = -27.833, zoom=4) %>%
      setMaxBounds(lng1 = 200, lat1 = -50,
                   lng2 = 60, lat2 = 0) %>%
      addMarkers(lng = df_main$long,
                 lat = df_main$lat,
                 clusterOptions = markerClusterOptions(),
                 label=paste(df_main$service_name, df_main$home_care_places))
      # %>%
      # addLegend("topright",
      #           pal = factpal,
      #           values = df_popn$Count,
      #           title = "Member Count",
      #           opacity = 1)
  
  }) # output$world_map
  
}

# Run the application 
shinyApp(ui = ui, server = server)


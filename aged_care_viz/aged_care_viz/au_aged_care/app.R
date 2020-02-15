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
countries <- readOGR("./au-postcodes-Visvalingam-0.1.geojson",
                          layer="au-postcodes-Visvalingam-0.1")

# countries <- readOGR(dsn=path.expand("accor_countries.geojson"))
# print(countries)
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
                      label = "Go")
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("World Map",
                   leafletOutput(outputId = "world_map")
          ),
          id = "tabs"
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # WORLD MAP
  output$world_map <- renderLeaflet({
    leaflet(countries,
            options = leafletOptions(dragging = TRUE,
                                     minZoom = 2)
            ) %>%
      
      # leaflet(countries) %>%
      addTiles() %>%
      # addProviderTiles("OpenStreetMap.BlackAndWhite") 
      addPolygons(weight=1, smoothFactor = 0.2, fillOpacity = 0.2) %>%
      # clearMarkers()  %>%
      setView(lng = 133.583, lat = -27.833, zoom=2) %>%
      setMaxBounds(lng1 = 200, lat1 = -50,
                   lng2 = 60, lat2 = 0)
      # setMaxBounds(lng1 = 147.8, lat1 = -43.21,
      #              lng2 = 117.2, lat2 = -12.5)
      # %>%
      # addAwesomeMarkers(lng = df_popn$Longitude,
      #                   lat = df_popn$Latitude,
      #                   icon = awesomeIcons(icon = 'plane',
      #                                       iconColor = 'black',
      #                                       library = 'ion',
      #                                       markerColor = df_popn$Color
      #                   ),
      #                   label = paste(df_popn$MainCity, df_popn$Count),
      #                   popup = paste(df_popn$MainCity, df_popn$Count)
      # )
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


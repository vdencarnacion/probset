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
df_main <- preprocess_au_service_list('./Australia-30-June-2019-v2-1.csv',
                                      './Australian_Post_Codes_Lat_Lon.csv')

ui <- fluidPage(
   
   # Application title
   titlePanel("Australian Aged Care Units"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "cluster_acu",
                    label = "Cluster Aged Care Units by:",
                    choices = c("Postcode", "Geographical"),
                    multiple = FALSE,
                    selected = c("Geographical")),

         actionButton(inputId = "go",
                      label = "Go")
         # width = 2
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
  xdf_local <- reactive({
    input_cluster_acu = input$cluster_acu
    if (input_cluster_acu == 'Postcode'){
      df_main['lat'] = df_main['postcode_lat']
      df_main['lon'] = df_main['postcode_lon']
    } else if (input_cluster_acu == 'Geographical') {
      df_main['lat'] = df_main['geoj_lat']
      df_main['lon'] = df_main['geoj_lon']
    }
    
    return (df_main)
  })

  # AUSTRALIAN MAP
  output$world_map <- renderLeaflet({
    input$go
    df_local = xdf_local()
    isolate({
      leaflet(postcodes,
              options = leafletOptions(dragging = TRUE,
                                       minZoom = 2)
      ) %>%
      addTiles() %>%
      addPolygons(weight=1, smoothFactor = 0.2, fillOpacity = 0) %>%
      # clearMarkers()  %>%
      setView(lng = 133.583, lat = -27.833, zoom=4) %>%
      setMaxBounds(lng1 = 200, lat1 = -50,
                   lng2 = 60, lat2 = 0) %>%
      addMarkers(lng = df_main$geoj_lon,
                 lat = df_main$geoj_lat,
                 clusterOptions = markerClusterOptions(),
                 label=paste(df_main$service_name, df_main$home_care_places))
      # %>%
      # addLegend("topright",
      #           pal = factpal,
      #           values = df_popn$Count,
      #           title = "Member Count",
      #           opacity = 1)
    })
  
  }) # output$world_map
  
}

# Run the application 
shinyApp(ui = ui, server = server)


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

# GeoJSON
# postcodes <- readOGR("./au-postcodes-Visvalingam-0.1.geojson",
#                      layer="au-postcodes-Visvalingam-0.1")
# lga_gda <- readOGR("./lga11aAust_0.1.geojson",
#                    layer="lga11aAust_0.1")
lga_gda <- readOGR("./lga11aAust_final.geojson",
                   layer="spDf")
# Data
# countries <- readOGR("./countries.geojson", layer="accor_countries")
df_main <- preprocess_au_service_list('./Australia-30-June-2019-v2-1.csv',
                                      './Australian_Post_Codes_Lat_Lon.csv')

ui <- fluidPage(
   
   # Application title
   titlePanel("Australian Aged Care Units"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "age_range",
                    label = "Age range:",
                    choices = c("ages35andUp",
                                "ages40andUp",
                                "ages45andUp",
                                "ages50andUp",
                                "ages55andUp",
                                "ages60andUp",
                                "ages65andUp",
                                "ages70andUp",
                                "ages75andUp",
                                "ages80andUp",
                                "ages85andUp"),
                    multiple = FALSE,
                    selected = c("ages35andUp")),

         actionButton(inputId = "go",
                      label = "Go")
         # width = 2
      ),

      mainPanel(
        tabsetPanel(
          tabPanel("Australian Map",
                   leafletOutput(outputId = "population")
          ),
          # tabPanel("Clustered ACUs",
          #          leafletOutput(outputId = "world_map")
          # ),
          id = "tabs"
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  xdf_local <- reactive({
    input_age_range = input$age_range
    if (input_age_range == 'ages35andUp'){
      df_main$total_persons = df_main$ages35andUp
    } else if (input_age_range == 'ages40andUp') {
      df_main$total_persons = df_main$ages40andUp
    } else if (input_age_range == 'ages45andUp') {
      df_main$total_persons = df_main$ages45andUp
    } else if (input_age_range == 'ages50andUp') {
      df_main$total_persons = df_main$ages50andUp
    } else if (input_age_range == 'ages55andUp') {
      df_main$total_persons = df_main$ages55andUp
    } else if (input_age_range == 'ages60andUp') {
      df_main$total_persons = df_main$ages60andUp
    } else if (input_age_range == 'ages65andUp') {
      df_main$total_persons = df_main$ages65andUp
    } else if (input_age_range == 'ages70andUp') {
      df_main$total_persons = df_main$ages70andUp
    } else if (input_age_range == 'ages75andUp') {
      df_main$total_persons = df_main$ages75andUp
    } else if (input_age_range == 'ages80andUp') {
      df_main$total_persons = df_main$ages80andUp
    } else if (input_age_range == 'ages85andUp') {
      df_main$total_persons = df_main$ages85andUp
    }
    
    return (df_main)
  })
  
  # AUSTRALIAN MAP (CLUSTERED ACUs)
  output$population <- renderLeaflet({
    input$go
    isolate({
      df_local = xdf_local()
      # pal <- colorBin("Greens", lga_gda$total_persons, 4, pretty = TRUE)
      pal <- colorQuantile("Greens", lga_gda$total_persons, n = 5)
      isolate({
        leaflet(lga_gda,
                options = leafletOptions(dragging = TRUE,
                                         minZoom = 2)
        ) %>%
        addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7, color=~pal(total_persons)) %>%
        addLegend("bottomright", pal = pal, values = ~total_persons,
                  title = "Population by Age",
                  # labFormat = labelFormat(prefix = "$"),
                  opacity = 1
        ) %>%
        addTiles() %>%
        setView(lng = 133.583, lat = -27.833, zoom=4) %>%
        setMaxBounds(lng1 = 200, lat1 = -50,
                     lng2 = 60, lat2 = 0) %>%
        clearMarkers()  %>%
        addMarkers(lng = df_local$geoj_lon,
                   lat = df_local$geoj_lat,
                   clusterOptions = markerClusterOptions(),
                   label=paste(df_local$service_name, df_local$home_care_places))
      })
    })
    
  })

  # AUSTRALIAN MAP (CLUSTERED ACUs)
  # output$world_map <- renderLeaflet({
  #   input$go
  #   df_local = xdf_local()
  #   isolate({
  #     leaflet(postcodes,
  #             options = leafletOptions(dragging = TRUE,
  #                                      minZoom = 2)
  #     ) %>%
  #     addTiles() %>%
  #     addPolygons(weight=1, smoothFactor = 0.2, fillOpacity = 0) %>%
  #     # clearMarkers()  %>%
  #     setView(lng = 133.583, lat = -27.833, zoom=4) %>%
  #     setMaxBounds(lng1 = 200, lat1 = -50,
  #                  lng2 = 60, lat2 = 0) %>%
  #     addMarkers(lng = df_main$geoj_lon,
  #                lat = df_main$geoj_lat,
  #                clusterOptions = markerClusterOptions(),
  #                label=paste(df_main$service_name, df_main$home_care_places))
  #   })
  # 
  # }) # output$world_map
  
}

# Run the application 
shinyApp(ui = ui, server = server)


library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(tidyverse)
library(leaflet)
library(geojsonR)
library(rgdal)
library(dplyr)

source('preprocess.R')

# GeoJSON
lga_gda <- readOGR("./lga11aAust_final.geojson",
                   layer="spDf")
# Data
df_main <- preprocess_au_service_list('./Australia-30-June-2019-v2-1.csv',
                                      './Australian_Post_Codes_Lat_Lon.csv')
df_popn_by_state_aggregated = read_csv('./df_popn_by_state_aggregated.csv')

ui <- fluidPage(
   
   # Application title
   titlePanel("Australian Aged Care Units"),
   
   sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "age_range",
                    label = "Age range:",
                    choices = c("ages60andUp",
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
                   leafletOutput(outputId = "population")),
          tabPanel("Population by Age (State)",
                   plotlyOutput(outputId = "bargraph_age_range_by_state")),
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

  xdf_popn <- reactive({
    input_age_range = input$age_range

    df_popn = df_popn_by_state %>%
      gather(key=age_range, value="popn_by_age_range", -abv_state)
    df_popn$age_range = as.factor(df_popn$age_range)
    df_popn$abv_state = as.factor(df_popn$abv_state)

    if (input_age_range == 'ages60andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages60andUp', ]
    } else if (input_age_range == 'ages65andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages65andUp', ]
    } else if (input_age_range == 'ages70andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages70andUp', ]
    } else if (input_age_range == 'ages75andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages75andUp', ]
    } else if (input_age_range == 'ages80andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages80andUp', ]
    } else if (input_age_range == 'ages85andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages85andUp', ]
    }
    
    return (df_popn)
  })
  
  # AUSTRALIAN MAP (CLUSTERED ACUs)
  output$population <- renderLeaflet({
    input$go
    isolate({
      df_local = xdf_local()
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
        # addCircleMarkers(lng = df_local$geoj_lon,
        #       lat = df_local$geoj_lat)
        addMarkers(lng = df_local$geoj_lon,
                   lat = df_local$geoj_lat,
                   clusterOptions = markerClusterOptions(),
                   label=paste(df_local$service_name, df_local$home_care_places))
        
      })
    })

  })
  
  output$bargraph_age_range_by_state <- renderPlotly({
    xdf_popn = xdf_popn()
    p = ggplot(xdf_popn, aes(x=abv_state, y=popn_by_age_range, fill=abv_state)) +
      geom_bar(stat="sum") +
      xlab("State") +
      ylab("Total Population")
    ggplotly(p, height=500)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

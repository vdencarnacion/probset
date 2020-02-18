library(shiny)
library(ggplot2)
library(plotly)
library(tidyr)
library(tidyverse)
library(leaflet)
library(geojsonR)
library(rgdal)
library(dplyr)
library(DT)

source('preprocess.R')

plotly.templates.default = "plotly_white"

# GeoJSON
# lga_gda <- readOGR("./lga11aAust_final.geojson",
#                    layer="spDf")
# Data
df_main <- preprocess_au_service_list('./Australia-30-June-2019-v2-1.csv',
                                      './Australian_Post_Codes_Lat_Lon.csv')
df_popn_by_state_aggregated = read_csv('./df_popn_by_state_aggregated.csv')
df_popn_by_lga_aggregated = read_csv('./df_popn_by_lga_aggregated.csv')
df_acu_by_state_aggregated = read_csv('./df_acu_by_state_aggregated.csv')
df_acu_by_suburb_aggregated = read_csv('./df_acu_by_suburb_aggregated.csv')
# print(df_popn_by_state_aggregated)

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
                    selected = c("ages50andUp")),

         actionButton(inputId = "go",
                      label = "Go"),
         width = 3
      ),

      mainPanel(
        tabsetPanel(
          # tabPanel("Australian Map",
          #          leafletOutput(outputId = "population")),
          tabPanel("Population by Age (State)",
                   plotlyOutput(outputId = "bargraph_age_range_by_state"),
                   DT::dataTableOutput(outputId='df_acu_by_state')),
          # tabPanel("Population by Age (Local Government Area)",
          #          DT::dataTableOutput(outputId='df_popn_by_lga'),
          #          DT::dataTableOutput(outputId='df_acu_by_suburb')),
          id = "tabs"
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  xdf_acu <- reactive({
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

  xdf_popn_by_age <- reactive({
    input_age_range = input$age_range

    df_popn = df_popn_by_state_aggregated %>%
      gather(key=age_range, value="popn_by_age_range", -abv_state)
    df_popn$age_range = as.factor(df_popn$age_range)
    df_popn$abv_state = as.factor(df_popn$abv_state)
    
    if (input_age_range == 'ages35andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages35andUp', ]
    } else if (input_age_range == 'ages40andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages40andUp', ]
    } else if (input_age_range == 'ages45andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages45andUp', ]
    } else if (input_age_range == 'ages50andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages50andUp', ]
    } else if (input_age_range == 'ages55andUp') {
      df_popn = df_popn[df_popn$age_range == 'ages55andUp', ]
    } else if (input_age_range == 'ages60andUp') {
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
  
  xdf_popn_perc_by_state <- reactive({
    input_age_range = input$age_range
    
    df_popn = df_popn_by_state_aggregated %>% data.frame()
    if (input_age_range == 'ages35andUp') {
      df_popn = df_popn[, c('abv_state', 'ages35andUp')]
      df_popn$distrib = df_popn$ages35andUp / sum(df_popn$ages35andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages40andUp') {
      df_popn = df_popn[, c('abv_state', 'ages40andUp')]
      df_popn$distrib = df_popn$ages40andUp / sum(df_popn$ages40andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages45andUp') {
      df_popn = df_popn[, c('abv_state', 'ages45andUp')]
      df_popn$distrib = df_popn$ages45andUp / sum(df_popn$ages45andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages50andUp') {
      df_popn = df_popn[, c('abv_state', 'ages50andUp')]
      df_popn$distrib = df_popn$ages50andUp / sum(df_popn$ages50andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages55andUp') {
      df_popn = df_popn[, c('abv_state', 'ages55andUp')]
      df_popn$distrib = df_popn$ages55andUp / sum(df_popn$ages55andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages60andUp') {
      df_popn = df_popn[, c('abv_state', 'ages60andUp')]
      df_popn$distrib = df_popn$ages60andUp / sum(df_popn$ages60andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages65andUp') {
      df_popn = df_popn[, c('abv_state', 'ages65andUp')]
      df_popn$distrib = df_popn$ages65andUp / sum(df_popn$ages65andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages70andUp') {
      df_popn = df_popn[, c('abv_state', 'ages70andUp')]
      df_popn$distrib = df_popn$ages70andUp / sum(df_popn$ages70andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages75andUp') {
      df_popn = df_popn[, c('abv_state', 'ages75andUp')]
      df_popn$distrib = df_popn$ages75andUp / sum(df_popn$ages75andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages80andUp') {
      df_popn = df_popn[, c('abv_state', 'ages80andUp')]
      df_popn$distrib = df_popn$ages80andUp / sum(df_popn$ages80andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    } else if (input_age_range == 'ages85andUp') {
      df_popn = df_popn[, c('abv_state', 'ages85andUp')]
      df_popn$distrib = df_popn$ages85andUp / sum(df_popn$ages85andUp)
      colnames(df_popn) = c('State', 'Population', '%Popn Distribution')
    }
    return (df_popn)
  })
  
  # AUSTRALIAN MAP (CLUSTERED ACUs)
  # output$population <- renderLeaflet({
  #   input$go
  #   isolate({
  #     xdf_acu = xdf_acu()
  #     pal <- colorQuantile("Greens", lga_gda$total_persons, n = 5)
  #     isolate({
  #       leaflet(lga_gda,
  #               options = leafletOptions(dragging = TRUE,
  #                                        minZoom = 2)
  #       ) %>%
  #       addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.7, color=~pal(total_persons)) %>%
  #       addLegend("bottomright", pal = pal, values = ~total_persons,
  #                 title = "Population by Age",
  #                 # labFormat = labelFormat(prefix = "$"),
  #                 opacity = 1
  #       ) %>%
  #       addTiles() %>%
  #       setView(lng = 133.583, lat = -27.833, zoom=4) %>%
  #       setMaxBounds(lng1 = 200, lat1 = -50,
  #                    lng2 = 60, lat2 = 0) %>%
  #       clearMarkers()  %>%
  #       # addCircleMarkers(lng = xdf_acu$geoj_lon,
  #       #       lat = xdf_acu$geoj_lat)
  #       addMarkers(lng = xdf_acu$geoj_lon,
  #                  lat = xdf_acu$geoj_lat,
  #                  clusterOptions = markerClusterOptions(),
  #                  label=paste(xdf_acu$service_name, xdf_acu$home_care_places))
  #       
  #     })
  #   })
  # 
  # })
  
  output$bargraph_age_range_by_state <- renderPlotly({

    xdf_popn_by_age = xdf_popn_by_age()
    p = xdf_popn_by_age %>%
      mutate(pct = prop.table(popn_by_age_range)) %>%
      ggplot(aes(x = abv_state,
                 y = popn_by_age_range,
                 label = scales::percent(pct))) +
      geom_text(position = position_dodge(width = .9),    # move to center of bars
                vjust = -1,    # nudge above top of bar
                size = 3) +
      geom_bar(stat='sum',
               fill='#ffcccb') +
      xlab("State") +
      ylab("Total Population")
    ggplotly(p)
    # })
    
  })

  output$df_acu_by_state <- DT::renderDataTable({
    # isolate({
    #   input$go
    df_popn_perc_by_state = xdf_popn_perc_by_state()
    
    colnames(df_acu_by_state_aggregated) = c('State',
                                             'ACU Count',
                                             'Home Care Places',
                                             'Residential Places',
                                             'Restorative Care Places',
                                             'Total Capacity',
                                             '%HCP', '%RP', '%RCP', '%Total ACU Capacity')
    merged_df = merge(df_acu_by_state_aggregated,
                      df_popn_perc_by_state,
                      by="State", all.x=TRUE)
    merged_df$Ratio = merged_df[, c('%Popn Distribution')]
    merged_df$Ratio2 = merged_df[, c("%Total ACU Capacity")]
    merged_df$Ratio = merged_df$Ratio / merged_df$Ratio2
    merged_df$Ratio2 = NULL
    colnames(merged_df) = c('State',
                           'ACU Count',
                           'Home Care Places',
                           'Resi- dential Places',
                           'Resto- rative Care Places',
                           'Total Capacity',
                           '%HCP', '%RP', '%RCP', '%Total ACU Capacity',
                           'Total Population',
                           '%Popn Distribution',
                           'Ratio %Popn-%ACU')
    merged_df = merged_df[, c('State',
                              'ACU Count',
                              'Home Care Places',
                              'Resi- dential Places',
                              'Resto- rative Care Places',
                              'Total Capacity',
                              '%HCP', '%RP', '%RCP',
                              '%Total ACU Capacity',
                              '%Popn Distribution',
                              'Ratio %Popn-%ACU')]
    # df_acu$Ratio = df_acu$ACUCount / df_acu$TotalCapacity
    df_acu = DT::datatable(merged_df, rownames=FALSE) %>% 
      formatPercentage(c('%HCP', '%RP', '%RCP',
                         '%Total ACU Capacity',
                         '%Popn Distribution'), 
                       2) %>%
      formatRound(c('Ratio %Popn-%ACU'), 2)
    
    return (df_acu)
    # })
  }, rownames=FALSE)
  
  output$df_popn_by_lga <- DT::renderDataTable({
    df_popn_lga = DT::datatable(df_popn_by_lga_aggregated, rownames=FALSE) # %>% 
      # formatPercentage(c('hcp_perc', 'rp_perc', 'rcp_perc', 'total_perc'), 
                       # 6)
    
    return (df_popn_lga)
  }, rownames=FALSE)

  output$df_acu_by_suburb <- DT::renderDataTable({
    colnames(df_acu_by_suburb_aggregated) = c('state',
                                              'suburb',
                                              'count',
                                              'home care places',
                                              'residential places',
                                              'restorative care places',
                                              'total',
                                              '%hcp', '%rp', '%rcp', '%total')
    df_acu_by_suburb = DT::datatable(df_acu_by_suburb_aggregated, rownames=FALSE) %>%
      formatPercentage(c('%hcp', '%rp', '%rcp', '%total'),
                       6)
    
    return (df_acu_by_suburb)
  }, rownames=FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)

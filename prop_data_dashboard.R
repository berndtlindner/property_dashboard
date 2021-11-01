library(shiny)
library(leaflet)
library(RColorBrewer)
library(shinydashboard)


data <- read.csv("data.csv") 
# %>% filter(!is.na(floor_size))
data$floor_size <- ifelse(data$floor_size > 10000, mean(data$floor_size, na.rm=TRUE), data$floor_size)

data$floor_size_plot <- log(data$floor_size)
data$floor_size_plot <- ifelse(is.na(data$floor_size_plot), mean(data$floor_size_plot, na.rm=TRUE), data$floor_size_plot)
data$floor_size_plot <- data$floor_size_plot*3


pal <- colorNumeric(
  palette = "RdYlGn",
  domain = data$price_data,
  reverse = TRUE
)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sliderInput("price_range", "Price", min(data$price_data, na.rm = TRUE), max(data$price_data, na.rm = TRUE),
                                 value = range(data$price_data), step = 100000),
    sliderInput("size_range", "Size (m2)", min(data$floor_size, na.rm = TRUE), max(data$floor_size, na.rm = TRUE),
                value = range(data$floor_size), step = 1)
    ),
  dashboardBody(leafletOutput("map", height = 700))
)

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    
    data[
      data$price_data >= input$price_range[1] & data$price_data <= input$price_range[2] &
      data$floor_size >= input$size_range[1] & data$floor_size <= input$size_range[2]
         ,]
  })
  
  output$map <- renderLeaflet({
    leaflet(data) %>% addTiles() %>%
      fitBounds(~min(lon), ~min(lat), ~max(lon), ~max(lat)) %>%
      #https://rstudio.github.io/leaflet/legends.html
      addLegend("bottomright", pal = pal, values = ~price_data,
                title = "Price",
                labFormat = labelFormat(prefix = "R"),
                opacity = 0.7
      ) %>%
      addLayersControl(
        baseGroups = c( "Toner", "Toner Lite", "OSM (default)"),
        options = layersControlOptions(collapsed = FALSE)
      ) %>% 
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite")
  })
  
  
  observe({
    
    leafletProxy("map", data = filteredData()) %>% 
      clearShapes() %>% 
      clearMarkers() %>% 
      # addCircles(
      addCircleMarkers(
        radius=~data$floor_size_plot, 
                 color=~pal(price_data),
                 label=~paste(floor_size, " m2", "|",
                              "R", format(price_data, big.mark=",")
                 ),
                 # https://steemit.com/geomatics/@thornux/leaflet-mapping-in-rstudio-custom-popups
                # https://github.com/electron/electron/issues/656
                 popup = ~paste0("<a href=",url, 
                                 '>link</a>',
                                 # "<iframe width=1000 height=500 src='", url ,
                                 # "'frameborder=0 allowfullscreen sandbox='allow-forms allow-popups allow-pointer-lock allow-same-origin allow-scripts'></iframe>", 
                                 sep=""),
                 #popup = ~paste('<iframe width="300" height="169" src=', url, 'frameborder="0" allowfullscreen></iframe>')
                 #popup = '<a href="https://www.property24.com/for-sale/strandvale/strand/western-cape/16514/108328732?plId=516899&plt=3/">P24</a>',
                 stroke = FALSE, 
                 fillOpacity = 0.5
      ) 
  })
  
}

shinyApp(ui, server)

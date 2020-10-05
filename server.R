#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(roll)
library(magrittr)
library(htmltools)
library(leaflet)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        output$myMap <- renderLeaflet({
            # Use leaflet() here, and only include aspects of the map that
            # won't need to change dynamically (at least, not unless the
            # entire map is being torn down and recreated).
            leaflet() %>%
                addTiles() %>%
                setView(lng = 9.501785, lat = 56.26392, zoom = 7) %>%
                addProviderTiles(providers$CartoDB.Positron)
            
        })    
        # input$file1 will be NULL initially. After the user selects and uploads a 
        # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
        # columns. The 'datapath' column will contain the local filenames where the 
        # data can be found.
        
        inFile <- input$file1
      
        if (is.null(inFile))
            return(NULL)
        
        dc <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
        
        observe({
          leafletProxy("myMap", data = dc) %>%
            addPolygons(
              color = "#444444", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0.5,
              fillColor = ~ colorQuantile("YlOrRd", dcr7dPer100k)(dcr7dPer100k), # changing the municipality color based on a specific variable
              label = dc$kommune
            ) %>% addCircleMarkers(lng = ~X, lat = ~Y, radius = 6, data = a_one_date, weight = 1, stroke = F, fillOpacity = .5, color = "#808080")
        })
    })

#https://github.com/stefanocudini/leaflet-panel-layers


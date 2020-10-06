#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        # input$file1 will be NULL initially. After the user selects and uploads a 
        # file, it will be a data frame with 'name', 'size', 'type', and 'datapath' 
        # columns. The 'datapath' column will contain the local filenames where the 
        # data can be found.

        dataInput<-reactive({
          req(input$file1)
          inFile <- input$file1
          
          if (is.null(inFile))
            return(NULL)
          
          dc <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
          
        })
        
        finalInput<-reactive({
            ProcessData(dataInput())
        })
        
        output$myMap <- renderLeaflet({
            # Use leaflet() here, and only include aspects of the map that
            # won't need to change dynamically (at least, not unless the
            # entire map is being torn down and recreated).
          if(is.null(finalInput()))
          {
            return(leaflet() %>%
                addTiles() %>%
                setView(lng = 9.501785, lat = 56.26392, zoom = 7) %>%
                addProviderTiles(providers$CartoDB.Positron))
          }
          else{
            leaflet(finalInput()) %>% 
              addPolygons(
                 data = dk, color = "#444444", weight = 1, smoothFactor = 0.5,
                 opacity = 1.0, fillOpacity = 0.5,
                 fillColor = ~ colorQuantile("YlOrRd", input$dcr7dPer100k)(input$dcr7dPer100k), # changing the municipality color based on a specific variable
             ) 
          }
        })  
    })

#https://github.com/stefanocudini/leaflet-panel-layers


#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/



# Define server logic required to draw a histogram


server <- (function(input, output) {

  # input$file1 will be NULL initially. After the user selects and uploads a
  # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
  # columns. The 'datapath' column will contain the local filenames where the
  # data can be found.

  dataInput <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath, header = TRUE, sep = ";", quote = "")
  })
  
  finalInput<-reactive({
    req(input$file1)
    ProcessData(dataInput())
  })
  

  output$map <- renderLeaflet({
    if (is.null(finalInput())) {
      return(NULL)
    }
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    dk_data<-finalInput()
    
    map<-leaflet(data = dk_data) %>%
      addTiles() %>%
      setView(lng = 9.501785, lat = 56.26392, zoom = 7) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = dk, color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.5,
        fillColor = ~colorQuantile("YlOrRd", dk_data$dcr7dPer100k)(dk_data$dcr7dPer100k)) %>%
          addCircleMarkers(lng = dk_data$X, lat = dk_data$Y, radius = 8, data = dk_data,
                           weight = 1, stroke = F, fillOpacity = 1, color = "#808080")
    for (i in 1:nrow(dk_data)) {
      map <- map %>%
        addPolylines(
          data = dk_data[i, ],
          lng = ~ c(X, custlng_Ch7),
          lat = ~ c(Y, custlat_Ch7),
          color = "white",
          opacity = 1,
          weight = 5,
          group = "Change from yesterday",
        )
    }
             for (i in 1:nrow(dk_data)) {
            map <- map %>%
                    addPolylines(
                      data = dk_data[i, ],
                      lng = ~ c(X, custlng_Ch7),
                      lat = ~ c(Y, custlat_Ch7),
                      color = ~dcr7dPer100kCh7Col,
                      
                      weight = 4,
                      group = "Change from yesterday",
                    )
          }

          for (i in 1:nrow(dk_data)) {
            map <- map %>%
              addPolylines(
                data = dk_data[i, ],
                lng = ~ c(X, custlng_Ch3),
                lat = ~ c(Y, custlat_Ch3),
                color = ~dcr7dPer100kCh3Col,
                weight = 4,
                group = "Change from 3 days ago",
              )
          }
          for (i in 1:nrow(dk_data)) {
            map <- map %>%
              addPolylines(
                data = dk_data[i, ],
                lng = ~ c(X, custlng_Ch1),
                lat = ~ c(Y, custlat_Ch1),
                color = ~dcr7dPer100kCh1Col,
                weight = 4,
                group = "Change from 7 days ago",
              )
          }
    map%>%
          addLayersControl(
                    overlayGroups = c("Change from yesterday", "Change from 3 days ago", "Change from 7 days ago"),
                    options = layersControlOptions(collapsed = FALSE),
                  )
  
  })


    # observe({
    #   dk_data<-finalInput()
    #   leafletProxy("myMap") %>%

    # })

#   observe({
#     leafletProxy("myMap", data = dataInput()) %>%
#       
#   })
# 
#   observe({
#     
#   })
# 
#   observe({
#     for (i in 1:nrow(a_one_date)) {
#         leafletProxy("myMap", data = dataInput()) %>%
#         addPolylines(
#           data = a_one_date[i, ],
#           lng = ~ c(X, custlng_Ch3),
#           lat = ~ c(Y, custlat_Ch3),
#           color = ~dcr7dPer100kCh7Col,
#           weight = 4,
#           group = "Change the last 3 days",
#         )
#     }
#   })
# 
#   observe({
#     for (i in 1:nrow(a_one_date)) {
#         leafletProxy("myMap", data = dataInput()) %>%
#         addPolylines(
#           data = a_one_date[i, ],
#           lng = ~ c(X, custlng_Ch7),
#           lat = ~ c(Y, custlat_Ch7),
#           color = ~dcr7dPer100kCh7Col,
#           weight = 4,
#           group = "Change the last 7 days",
#         )
#     }
#   })
# 
#   observe({
#     myMap<- leafletProxy("myMap", data = dataInput()) %>%
#       addLayersControl(
#         overlayGroups = c("Change since the day before", "Change the last 3 days", "Change the last 7 days"),
#         options = layersControlOptions(collapsed = FALSE),
#       )
#   })
  })


# https://github.com/stefanocudini/leaflet-panel-layers

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

  dataInput <- eventReactive(input$file1,{
    req(input$file1)
    inFile <- input$file1
    
    if (is.null(inFile)) {
      return(NULL)
    }
    dc <- read.csv(inFile$datapath, header = input$header, sep = input$sep, quote = input$quote)
    
    ProcessData(dc)
  })

  output$myMap <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addTiles() %>%
      setView(lng = 9.501785, lat = 56.26392, zoom = 7) %>%
      addProviderTiles(providers$CartoDB.Positron)
  })


    observe({
      leafletProxy("myMap", data = dataInput()) %>%
        addPolygons(
          data = df_dk_covid, color = "#444444", weight = 1, smoothFactor = 0.5,
          opacity = 1.0, fillOpacity = 0.5,
          fillColor = ~colorQuantile("YlOrRd", dcr7dPer100k)(dcr7dPer100k)
        )
    })

#   observe({
#     leafletProxy("myMap", data = dataInput()) %>%
#       addCircleMarkers(lng = ~X, lat = ~Y, radius = 6, data = a_one_date, weight = 1, stroke = F, fillOpacity = .5, color = "#808080")
#   })
# 
#   observe({
#     for (i in 1:nrow(a_one_date)) {
#     leafletProxy("myMap", data = dataInput()) %>%
#         addPolylines(
#           data = a_one_date[i, ],
#           lng = ~ c(X, custlng_Ch1),
#           lat = ~ c(Y, custlat_Ch1),
#           color = ~dcr7dPer100kCh1Col,
#           weight = 4,
#           group = "Change since the day before",
#         )
#     }
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

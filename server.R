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
    read.csv(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote)
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
    
    
    
    dk_pop <-
      dsize %>%
      left_join(dk, by = c("kommune" = "NAME_2"))
    
    sf_dk <- st_as_sf(dk_pop, sf_column_name = "geometry")
    # getting the centroids to grab the coordinates from the dataset
    dk_cent <- st_centroid(sf_dk)
    dk_coords <- st_coordinates(dk_cent)
    
    # converting the matrix into a df again
    dk_coords_next <- as.data.frame(dk_coords)
    
    # adding the coordinates to the kommunes and their population
    dk_merge_coords <-
      dsize %>%
      cbind(dk_coords_next)
    
    # merging the coords/kommunes with the covid data
    dk_merge_coords_test <-
      dk_data %>%
      merge(dk_merge_coords)
    
    # merging the covid data into the shapefile to plot it
    df_dk_covid <-
      dk_data %>%
      dplyr::filter(date_sample == "2020-09-16") %>%
      merge(sf_dk)
    
    # to plot the data it needs to be a shapefile (sf) again - creating shapefile
    # and specifying where it should take sf data from
    sf_dk_covid <-
      st_as_sf(sf_dk_covid, sf_column_name = "geometry")
    
    # filtering for a single date to not cause overplotting
    a_one_date <- dk_merge_coords_test %>%
      dplyr::filter(date_sample == "2020-09-16")
    
    scaleFactor <- 20
    
    a_one_date$dcr7dPer100kCh1Col <- plyr::mapvalues(sign(a_one_date$dcr7dPer100kCh1), from = c(1, 0, -1), to = c("#FF0000", "#00FFFF", "#00FF00"))
    a_one_date$dcr7dPer100kCh3Col <- plyr::mapvalues(sign(a_one_date$dcr7dPer100kCh3), from = c(1, 0, -1), to = c("#FF0000", "#00FFFF", "#00FF00"))
    a_one_date$dcr7dPer100kCh7Col <- plyr::mapvalues(sign(a_one_date$dcr7dPer100kCh7), from = c(1, 0, -1), to = c("#FF0000", "#00FFFF", "#00FF00"))
    
    a_one_date %<>%
      mutate(
        y_dcr7dPer100kCh1 = (plogis(abs(dcr7dPer100kCh1) * 10) - 0.5) * 188,
        y_dcr7dPer100kCh3 = (plogis(abs(dcr7dPer100kCh3) * 10) - 0.5) * 188,
        y_dcr7dPer100kCh7 = (plogis(abs(dcr7dPer100kCh7) * 10) - 0.5) * 188,
        custlng_dcr7dPer100kCh1 = cos(deg2rad(y_dcr7dPer100kCh1)) / scaleFactor,
        custlng_dcr7dPer100kCh3 = cos(deg2rad(y_dcr7dPer100kCh3)) / scaleFactor,
        custlng_dcr7dPer100kCh7 = cos(deg2rad(y_dcr7dPer100kCh7)) / scaleFactor,
        custlat_dcr7dPer100kCh1 = sin(deg2rad(y_dcr7dPer100kCh1)) / scaleFactor,
        custlat_dcr7dPer100kCh3 = sin(deg2rad(y_dcr7dPer100kCh3)) / scaleFactor,
        custlat_dcr7dPer100kCh7 = sin(deg2rad(y_dcr7dPer100kCh7)) / scaleFactor,
        custlng_Ch1 = X + (custlng_dcr7dPer100kCh1),
        custlng_Ch3 = X + (custlng_dcr7dPer100kCh3),
        custlng_Ch7 = X + (custlng_dcr7dPer100kCh7),
        custlat_Ch1 = Y + sign(dcr7dPer100kCh1) * (custlat_dcr7dPer100kCh1),
        custlat_Ch3 = Y + sign(dcr7dPer100kCh3) * (custlat_dcr7dPer100kCh3),
        custlat_Ch7 = Y + sign(dcr7dPer100kCh7) * (custlat_dcr7dPer100kCh7)
      )
    
    map<-leaflet(data = dk_data) %>%
      addTiles() %>%
      setView(lng = 9.501785, lat = 56.26392, zoom = 7) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = df_dk_covid, color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 0.5,
        fillColor = ~colorQuantile("YlOrRd", dcr7dPer100k)(dcr7dPer100k)) %>%
          addCircleMarkers(lng = ~X, lat = ~Y, radius = 6, data = a_one_date,
                           weight = 1, stroke = F, fillOpacity = .5, color = "#808080")
          for (i in 1:nrow(a_one_date)) {
            map <- map %>%
                    addPolylines(
                      data = a_one_date[i, ],
                      lng = ~ c(X, custlng_Ch1),
                      lat = ~ c(Y, custlat_Ch1),
                      color = ~dcr7dPer100kCh1Col,
                      weight = 4,
                      group = "Change since the day before",
                    )
          }
          for (i in 1:nrow(a_one_date)) {
            map <- map %>%
              addPolylines(
                data = a_one_date[i, ],
                lng = ~ c(X, custlng_Ch3),
                lat = ~ c(Y, custlat_Ch3),
                color = ~dcr7dPer100kCh3Col,
                weight = 4,
                group = "Change the last 3 days",
              )
          }
          for (i in 1:nrow(a_one_date)) {
            map <- map %>%
              addPolylines(
                data = a_one_date[i, ],
                lng = ~ c(X, custlng_Ch7),
                lat = ~ c(Y, custlat_Ch7),
                color = ~dcr7dPer100kCh7Col,
                weight = 4,
                group = "Change the last 7 days",
              )
          }
    map%>%
          addLayersControl(
                    overlayGroups = c("Change since the day before", "Change the last 3 days", "Change the last 7 days"),
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

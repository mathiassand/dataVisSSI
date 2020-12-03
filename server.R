#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

server <- (function(input, output) {

  # input$file1 will be NULL initially. After the user selects and uploads a
  # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
  # columns. The 'datapath' column will contain the local filenames where the
  # data can be found.

  dataInput <- reactive({
    req(input$file1)
    dc <- read.csv(input$file1$datapath, header = TRUE, sep = ";", quote = "", encoding = "UTF-8")
    ProcessData(dc)
  })

  shapefile <- reactive({
    req(input$file1)
    Process_sf(dk)
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).

    sf_dk <- shapefile()
    dk_data <- dataInput()


    # merging the coords/kommunes with the covid data
    dk_merge_coords_test <-
      dk_data %>%
      merge(sf_dk)

    # merging the covid data into the shapefile to plot it
    df_dk_covid <-
      dk_data %>%
      dplyr::filter(date_sample == "2020-10-23") %>%
      merge(sf_dk)

    # to plot the data it needs to be a shapefile (sf) again - creating shapefile
    df_dk_covid <-
      st_as_sf(df_dk_covid, sf_column_name = "geometry")

    # filtering for a single date to not cause overplotting
    a_one_date <- dk_merge_coords_test %>%
      dplyr::filter(date_sample == "2020-10-23")

    scaleFactor <- 20

    a_one_date$dcr7dPer100kCh1Col <- plyr::mapvalues(sign(a_one_date$dcr7dPer100kCh1), from = c(1, 0, -1), to = c("#FF0000", "#00FFFF", "#00FF00"))
    a_one_date$dcr7dPer100kCh3Col <- plyr::mapvalues(sign(a_one_date$dcr7dPer100kCh3), from = c(1, 0, -1), to = c("#FF0000", "#00FFFF", "#00FF00"))
    a_one_date$dcr7dPer100kCh7Col <- plyr::mapvalues(sign(a_one_date$dcr7dPer100kCh7), from = c(1, 0, -1), to = c("#FF0000", "#00FFFF", "#00FF00"))

    deg2rad <- function(x) {
      radian <- x * pi / 180
      return(radian)
    }

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

    # img <- "https://logos-download.com/wp-content/uploads/2019/06/Aalborg_Universitet_Logo_white_text.png"
    
    bins <- c(0,10,20,30,40,50,10000)
      # seq(min(df_dk_covid$dcr7dPer100k)*100, max(df_dk_covid$dcr7dPer100k)*100, max(df_dk_covid$dcr7dPer100k)*100 / 5)
    #
    pal <- colorBin(
      palette = c("#ffe0c4", "#ffd3ab","#febe8d","#fea469", "#f4874c", "#e16d3d", "#a3573a"),
      bins = bins
    )

    a_one_date$zoom <- sample(1:99, size = length(a_one_date$kommune), replace = T)

    map <- leaflet(options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
      setView(lng = 11.001785, lat = 56.26392, zoom = 7.5) %>%
      addPolygons(
        data = df_dk_covid, color = "#444444", weight = 1, smoothFactor = 0.5,
        opacity = 1.0, fillOpacity = 1,
        # fillColor = ~ colorQuantile("YlOrRd", dcr7dPer100k)(dcr7dPer100k),
        fillColor = ~ pal(dcr7dPer100k), 
        popup = paste0(
          "<h5>Seneste 7 dage</h5>",
          "<br>",
          "<b>Kommune:</b> ",
          a_one_date$kommune,
          "<br>",
          "<b>Tilfælde:</b> ",
          a_one_date$dcr7d,
          "<br>",
          "<b>Incidens:</b> ",
          round(a_one_date$dcr7dPer100k, digits=0)
        ),
      ) %>%
      addCircleMarkers(
        lng = ~X, lat = ~Y, radius = 6, data = a_one_date,
        weight = 1, stroke = F, fillOpacity = .8, color = "#808080"
      ) %>%
      addLegend(
        data = a_one_date,
        position = "topright",
        pal = pal,
        values = ~dcr7dPer100k,
        title = "Incidens de seneste 7 dage"
      ) 
      #%>%
      # addLogo(
      #   img, 
      #   position = "bottomleft",
      #   width = 100,
      #   height = 100
      # )
    
    # addRectangles(
    #   lng1 = 11.8821, lat1 = 55.8711, 
    #   lng2 = 12.7746, lat2 = 55.5536, 
    #   fillColor = "transparent",
    # )

    for (i in 1:nrow(a_one_date)) {
      map <- map %>%
        addPolylines(
          data = a_one_date[i, ],
          lng = ~ c(X, custlng_Ch1),
          lat = ~ c(Y, custlat_Ch1),
          color = ~dcr7dPer100kCh1Col,
          weight = 4,
          opacity = 1,
          group = "Ændring fra i går"
        )
    }
    for (i in 1:nrow(a_one_date)) {
      map <- map %>%
        addPolylines(
          data = a_one_date[i, ],
          lng = ~ c(X, custlng_Ch1),
          lat = ~ c(Y, custlat_Ch1),
          color = "white",
          weight = 5,
          group = "Ændring fra i går"
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
          opacity = 1,
          group = "3 dage siden"
        )
    }
    for (i in 1:nrow(a_one_date)) {
      map <- map %>%
        addPolylines(
          data = a_one_date[i, ],
          lng = ~ c(X, custlng_Ch3),
          lat = ~ c(Y, custlat_Ch3),
          color = "white",
          weight = 5,
          group = "3 dage siden"
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
          opacity = 1,
          group = "7 dage siden"
        )
    }
    for (i in 1:nrow(a_one_date)) {
      map <- map %>%
        addPolylines(
          data = a_one_date[i, ],
          lng = ~ c(X, custlng_Ch7),
          lat = ~ c(Y, custlat_Ch7),
          color = "white",
          weight = 5,
          group = "7 dage siden"
        )
    }
    map %>%
      addLayersControl(
        position = "topright",
        overlayGroups = c("Ændring fra i går", "3 dage siden", "7 dage siden"),
        options = layersControlOptions(collapsed = FALSE),
      ) %>% hideGroup("3 dage siden") %>% hideGroup("7 dage siden")
  })
  

  # observe({
  #   click <- input$map_rectangle_click
  #   if(is.null(click))
  #     return()
  # 
  #   leafletProxy("map") %>%
  #     setView(lng = click$lng, lat = click$lat, zoom = 9)
  # })
# 
#   observe({
#     click <- input$map_click
#     if(is.null(click))
#       return()
# 
#     leafletProxy("map") %>%
#       setView(lng = 11.001785, lat = 56.26392, zoom = 7.5)
#   })
})

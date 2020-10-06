library(shiny)
library(roll)
library(magrittr)
library(htmltools)
library(leaflet)
library(sf)
library(roll)
library(magrittr)
library(here)
library(readr)
library(tidyr)

dsize <- read_delim(here("Municipality_test_pos.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
dk <- st_read("shapefiles/gadm36_DNK_2.shp")


ProcessData<-function(dc){
  
  {{dc}} %<>%
    pivot_longer(cols = !date_sample, names_to = "kommune", values_to = "casesDiagnosed") %>% arrange(kommune, date_sample)
  
  sum(is.na(dc$casesDiagnosed))
  
  dsize <- dsize %>%
    select(contains("Kom"), Befolkningstal) %>%
    rename("kID" = `Kommune_(id)`, "kommune" = `Kommune_(navn)`, population = Befolkningstal) %>%
    mutate(population = population * 1000) %>%
    select(-kID)
  
  dc[!(dc$kommune %in% dsize$kommune), ]$kommune
  dc$kommune <- str_replace(dc$kommune, "Copenhagen", "København")
  dc <- merge(dc, dsize)
  
  {{dc}} %<>%
    group_by(kommune) %>%
    mutate(
      casesDPer100k = casesDiagnosed / (population / 1000),
      dcr7d = roll_sum(casesDiagnosed, width = 7, min_obs = 1),
      dcr7dPer100k = dcr7d / (population / 1000),
      dcr7dPer100kCh1 = lag(dcr7dPer100k, 1) - dcr7dPer100k,
      dcr7dPer100kCh3 = lag(dcr7dPer100k, 3) - dcr7dPer100k,
      dcr7dPer100kCh7 = lag(dcr7dPer100k, 7) - dcr7dPer100k
    )
  
  dk$NAME_2 <- str_replace(dk$NAME_2, "Århus", "Aarhus")
  dk$NAME_2 <- str_replace(dk$NAME_2, "Høje Taastrup", "Høje-Taastrup")
  dk$NAME_2 <- str_replace(dk$NAME_2, "Vesthimmerland", "Vesthimmerlands")
  
  # joining the population to the shapefile 
  dk_pop <-
    dsize %>%
    left_join(dk, by = c("kommune" = "NAME_2"))
  
  # transforming the df into a shapefile again, that makes us use the geometry points
  df_dk <- st_as_sf(dk_pop, sf_column_name = "geometry")
  
  # getting the centroids to grab the coordinates from the dataset
  dk_cent <- st_centroid(df_dk)
  dk_coords <- st_coordinates(dk_cent)
  
  # converting the matrix into a df again
  dk_coords_next <- as.data.frame(dk_coords)
  
  # adding the coordinates to the kommunes and their population
  dk_merge_coords <-
    dsize %>%
    cbind(dk_coords_next)
  
  # merging the coords/kommunes with the covid data
  dk_merge_coords_test <-
    df %>%
    merge(dk_merge_coords)
  
  # merging the covid data into the shapefile to plot it
  df_dk_covid <-
    df %>%
    filter(date_sample == "2020-09-17") %>%
    merge(df_dk)
  
  # to plot the data it needs to be a shapefile (sf) again - creating shapefile
  # and specifying where it should take sf data from
  df_dk_covid <- 
    st_as_sf(df_dk_covid, sf_column_name = "geometry")
  
  #making another sf to plot copenhagen as a seperate map
  df_cph_covid<-
    df_dk_covid%>%
    filter(NAME_1=="Hovedstaden")
  
  # filtering for a single date to not cause overplotting
  a_one_date <- dk_merge_coords_test %>%
    filter(date_sample == "2020-09-17")
  
  
  ########### MAP###########
  
  # getting the absolute values - converting the negative values into 0
  # to later normalize
  
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
}




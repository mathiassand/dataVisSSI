library(shiny)
library(roll)
library(magrittr)
library(htmltools)
library(leaflet)
library(sf)
library(here)
# library(readr)
# library(tidyr)
library(tidyverse)
# library(stringr)


dsize <- read_delim("Municipality_test_pos.csv", ";", escape_double = FALSE, trim_ws = TRUE)
dt <- read_delim("Municipality_tested_persons_time_series.csv", ";", escape_double = FALSE, trim_ws = TRUE)
dk <- st_read("shapefiles/gadm36_DNK_2.shp")
# browser()

ProcessData <- function(dc) {
  
  deg2rad <- function(x) {
    radian <- x * pi / 180
    return(radian)
  }
  
  dc %<>%
    pivot_longer(cols = !date_sample, names_to = "kommune", values_to = "casesDiagnosed") %>% arrange(kommune, date_sample)
  
  dt %<>%
    pivot_longer(cols = !PrDate_adjusted, names_to = "kommune", values_to = "testsConducted") %>%
    arrange(kommune, PrDate_adjusted) %>%
    rename(date_sample = PrDate_adjusted)
  
  # check there are no NA in the set
  sum(is.na(dc$casesDiagnosed))
  sum(is.na(dt$testsConducted))
  sum(is.na(dt$testsConducted))
  sum(is.na(dsize$Befolkningstal))
  
  # make data compatible rename columns for merging and
  dsize <- dsize %>%
    select(contains("Kom"), Befolkningstal) %>%
    rename("kID" = `Kommune_(id)`, "kommune" = `Kommune_(navn)`, population = Befolkningstal) %>%
    mutate(population = population * 1000) %>%
    select(-kID)
  
  # check Kommune name can be used as key
  dc[!(dc$kommune %in% dsize$kommune), ]$kommune
  dsize[!(dsize$kommune %in% dt$kommune), ]$kommune
  dt[!(dt$kommune %in% dsize$kommune), ]$kommune
  
  # ooops!
  # for some reason one data frame uses København the other Copenhagen),
  dc$kommune <- str_replace(dc$kommune, "Copenhagen", "København")
  dt$kommune <- str_replace(dt$kommune, "Copenhagen", "København")
  # check again keys are clean OK re-run rows 35-37
  
  # merge data together
  dc <- merge(dc, dsize)
  dt <- merge(dt, dsize)
  
  # create new variables (e.g. rolling aggregates)
  dc %<>%
    group_by(kommune) %>%
    mutate(
      casesDPer100k = casesDiagnosed / (population / 1000),
      dcr7d = roll_sum(casesDiagnosed, width = 7, min_obs = 1),
      dcr7dPer100k = dcr7d / (population / 1000),
      dcr7dPer100kCh1 = lag(dcr7dPer100k, 1) - dcr7dPer100k,
      dcr7dPer100kCh3 = lag(dcr7dPer100k, 3) - dcr7dPer100k,
      dcr7dPer100kCh7 = lag(dcr7dPer100k, 7) - dcr7dPer100k
    )
  
  dt %<>%
    group_by(kommune) %>%
    mutate(
      testsPer100k = testsConducted / (population / 1000),
      tr7d = roll_sum(testsConducted, width = 7, min_obs = 1),
      tr7dP100k = tr7d / (population / 1000),
      tr7dP100kCh1 = lag(tr7dP100k, 1) - tr7dP100k,
      tr7dP100kCh3 = lag(tr7dP100k, 3) - tr7dP100k,
      tr7dP100kCh7 = lag(tr7dP100k, 7) - tr7dP100k
    )
  
  # merge test with case data
  # samsø doesn't exist in the dc dataset, which is why we cannot plot anything from it on the interactive map,
  # as we are only plotting changes in the confirmed cases (dc), and not tests conducted (dt)
  df <- merge(dc, dt)
  
  df$PosTestRate7d <- df$dcr7dPer100k / df$tr7dP100k
  
  # check there were no diagnosis without tests
  sum(df$casesDiagnosed > df$testsConducted)
  
  
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
    dc %>%
    merge(dk_merge_coords)

  # merging the covid data into the shapefile to plot it
  df_dk_covid <-
    dc %>%
    filter(date_sample == "2020-09-17") %>%
    merge(df_dk)

  # to plot the data it needs to be a shapefile (sf) again - creating shapefile
  # and specifying where it should take sf data from
   df_dk_covid <-
    st_as_sf(df_dk_covid, sf_column_name = "geometry")

  # filtering for a single date to not cause overplotting
  a_one_date <- dk_merge_coords_test %>%
    filter(date_sample == "2020-09-17")


  ########### MAP ###########

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



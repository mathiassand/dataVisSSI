library(shiny)
library(roll)
library(magrittr)
library(htmltools)
library(leaflet)
library(sf)
library(here)
library(readr)
library(tidyr)
library(tidyverse)
library(stringr)

dt <- read_delim(here("Municipality_tested_persons_time_series.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
dsize <- read_delim(here("Municipality_test_pos.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
dk <- st_read("shapefiles/gadm36_DNK_2.shp")
dk$NAME_2 <- str_replace(dk$NAME_2, "Århus", "Aarhus")
dk$NAME_2 <- str_replace(dk$NAME_2, "Høje Taastrup", "Høje-Taastrup")
dk$NAME_2 <- str_replace(dk$NAME_2, "Vesthimmerland", "Vesthimmerlands")

dsize <- dsize %>%
  select(contains("Kom"), Befolkningstal) %>%
  rename("kID" = `Kommune_(id)`, "kommune" = `Kommune_(navn)`, population = Befolkningstal) %>%
  mutate(population = population * 1000) %>%
  select(-kID)

ProcessData <- function(dc) {
  # re-formatting the dc dataframe
  dc %<>%
    pivot_longer(cols = !date_sample, names_to = "kommune", values_to = "casesDiagnosed") %>% arrange(kommune, date_sample)
  
  # replacing dots with hyphen
  dc$kommune <- gsub("\\.", "-", dc$kommune)

  # re-formatting the dt dataframe
  dt %<>%
    pivot_longer(cols = !PrDate_adjusted, names_to = "kommune", values_to = "testsConducted") %>%
    arrange(kommune, PrDate_adjusted) %>%
    rename(date_sample = PrDate_adjusted)
  
  # check Kommune name can be used as key
  unique(dc[!(dc$kommune %in% dsize$kommune), ]$kommune)
  unique(dsize[!(dsize$kommune %in% dc$kommune), ]$kommune)
  unique(dt[!(dt$kommune %in% dsize$kommune), ]$kommune)

  # ooops!
  # for some reason one data frame uses København the other Copenhagen),
  dc$kommune <- str_replace(dc$kommune, "Copenhagen", "København")

  # merge data together
  dc <- merge(dc, dsize)

  # create new variables (e.g. rolling aggregates)
  dc %<>%
    group_by(kommune) %>%
    mutate(
      casesDPer100k = casesDiagnosed / (population / 1000),
      dcr7d = roll_sum(casesDiagnosed, width = 3, min_obs = 1),
      dcr7dPer100k = dcr7d / (population / 1000),
      dcr7dPer100kCh1 = lag(dcr7dPer100k, 1) - dcr7dPer100k,
      dcr7dPer100kCh3 = lag(dcr7dPer100k, 3) - dcr7dPer100k,
      dcr7dPer100kCh7 = lag(dcr7dPer100k, 7) - dcr7dPer100k
    )

  return(dc)
}

Process_sf <- function(dk) {
  # including population to the shapefile
  dk_pop <-
    dsize %>%
    left_join(dk, by = c("kommune" = "NAME_2"))

  # transforming dk_pop to a shapefile
  sf_dk <- st_as_sf(dk_pop, sf_column_name = "geometry")

  # getting the centroids to grab the coordinates from the shapefile - results 
  # in a matrix
  dk_cent <- st_centroid(sf_dk)
  dk_coords <- st_coordinates(dk_cent)

  # converting the matrix into a df again
  dk_coords_next <- as.data.frame(dk_coords)

  # adding the coordinates to the kommunes and their population
  dk_merge_coords <-
    dsize %>%
    cbind(dk_coords_next)

  # merging the coordinates into the shapefile 
  dk_merge_coords <-
    dk_merge_coords %>%
    merge(sf_dk)

  return(dk_merge_coords)
}

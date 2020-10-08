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
library(tidyverse)
library(stringr)


dsize <- read_delim(here("Municipality_test_pos.csv"), ";", escape_double = FALSE, trim_ws = TRUE)
dt <- read_delim(here("Municipality_tested_persons_time_series.csv"), ";", escape_double = FALSE, trim_ws = TRUE)

dt %<>%
  pivot_longer(cols = !PrDate_adjusted, names_to = "kommune", values_to = "testsConducted") %>%
  arrange(kommune, PrDate_adjusted) %>%
  rename(date_sample = PrDate_adjusted)

dk <- st_read("shapefiles/gadm36_DNK_2.shp")
dk$NAME_2 <- str_replace(dk$NAME_2, "Århus", "Aarhus")
dk$NAME_2 <- str_replace(dk$NAME_2, "Høje Taastrup", "Høje-Taastrup")
dk$NAME_2 <- str_replace(dk$NAME_2, "Vesthimmerland", "Vesthimmerlands")

sum(is.na(dsize$Befolkningstal))

dsize <- dsize %>%
  select(contains("Kom"), Befolkningstal) %>%
  rename("kID" = `Kommune_(id)`, "kommune" = `Kommune_(navn)`, population = Befolkningstal) %>%
  mutate(population = population * 1000) %>%
  select(-kID)

dsize[!(dsize$kommune %in% dt$kommune), ]$kommune

ProcessData <- function(dc) {
  
  deg2rad <- function(x) {
    radian <- x * pi / 180
    return(radian)
  }
  
  dc %<>%
    pivot_longer(cols = !date_sample, names_to = "kommune", values_to = "casesDiagnosed") %>% arrange(kommune, date_sample)
  
  # check there are no NA in the set
  sum(is.na(dc$casesDiagnosed))
  sum(is.na(dt$testsConducted))

  # check Kommune name can be used as key
  dc[!(dc$kommune %in% dsize$kommune), ]$kommune
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
  
  


}



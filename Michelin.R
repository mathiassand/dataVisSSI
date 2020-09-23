install.packages("esquisse")
library(here)
library(tidyverse)
michelinStars<-read.csv(here("exercises","ws02","MichelinStars.csv"))
esquisse::esquisser()


michelinStars<-michelinStars %>% mutate(MRestaurantsPerMillion=total/(population/1000000),MRestaurantsPer1ksqkm=total/(area/1000)) %>% arrange(MRestaurantsPerMillion)


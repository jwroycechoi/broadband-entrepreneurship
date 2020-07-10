#### Spatial Regression Analysis and Mapping ####
install.packages("maptools", dependencies = T)
install.packages("spdep", dependencies = T)
install.packages("leaflet", dependencies = T)
install.packages("RColorBrewer", dependencies = T)
install.packages("tmaptools")
install.packages("sf", dependencies = T)
install.packages("sp", dependencies = T)

library(maptools)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(spdep)
library(tigris)
library(ggplot2)
library(sf)
library(sp)

#### Get spatial shapefile using tigris package ####
## State of Texas FIPS code is 48 ##
tx_county <- counties("Texas")
str(slot(tx_county, "data"))
leaflet(tx_county) %>% addPolygons() %>% addTiles()

class(tx_county)
tx_county_sf <- st_as_sf(tx_county)
class(tx_county_sf)
head(tx_county_sf)

#### Add the spatial element to the dataset ####
str(tx_county_sf)
str(tx_bb_entrepreneur_merged_v2)
tx_bb_entrepreneur_merged_v3 <- sp::merge(tx_county, tx_bb_entrepreneur_merged_v2,
                                          by.x = "GEOID", by.y = "FIPS")


class(tx_bb_entrepreneur_merged_v2)
class(tx_bb_entrepreneur_merged_v3)

#### Calculate spatial dependence matrix (Spatial weight) ####
## Basic introduction available at http://www.econ.uiuc.edu/~lab/workshop/Spatial_in_R.html ##

list_queen <- poly2nb(tx_bb_entrepreneur_merged_v3, queen = T)
list_rook <- poly2nb(tx_bb_entrepreneur_merged_v3, queen = F)
W <- nb2listw(list_queen, style = "W", zero.policy = T)
W
W_r <- nb2listw(list_rook, style = "W", zero.policy = T)
W_r

plot(W, coordinates(tx_bb_entrepreneur_merged_v3))
plot(W_r, coordinates(tx_bb_entrepreneur_merged_v3))



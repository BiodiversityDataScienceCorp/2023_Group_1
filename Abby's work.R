# rerun downloads
install.packages("dismo")
install.packages("maptools")
install.packages("tidyverse")
install.packages("rJava")
install.packages("maps")
install.packages("spocc")

library(dismo)
library(maptools)
library(tidyverse)
library(rJava)
library(maps)
library(spocc)


# get data from gbif:
myQuery<-occ(query="Rana boylii", from="gbif",limit=4000)

rana<-myQuery$gbif$data$Rana_boylii

rana

#clean data:
wrld<-ggplot2::map_data("world")

noAlaskaPoints <- rana %>% filter(longitude < 50)

noAlaskaPoints

rana %>% filter(latitude == "NA" | longitude == "NA") %>% summarise("NA") 

unique(is.na(rana$latitude))

noNA <- rana %>% filter(latitude!="NA"|longitude!="NA")

noAlaskaPoints <- noNA %>% filter(longitude < 50)

ranaDataNotCoords <- noAlaskaPoints %>%  select(longitude, latitude)

cleanFrog <- ranaDataNotCoords %>% filter(latitude!="NA"|longitude!="NA") %>%
  filter(latitude<50)

# Mapping:

ranaDataSpatialPts <- SpatialPoints(cleanFrog, proj4string = CRS("+proj=longlat"))

currentEnv <- getData("worldclim", var="bio", res=2.5)

climList <- list.files(path = "data/wc2-5/", pattern = ".bil$", # getting the climate data from the folder
                       full.names = T) 

clim <- raster::stack(climList)

plot(clim[[12]])

plot(ranaDataSpatialPts, add = TRUE)

mask <- raster(clim[[1]])

geographicExtent <- extent(x = ranaDataSpatialPts)

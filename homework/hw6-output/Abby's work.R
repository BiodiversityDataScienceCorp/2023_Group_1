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

#Data cleaning:
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

# Mapping!

# Current SDM
ranaDataSpatialPts <- SpatialPoints(cleanFrog, proj4string = CRS("+proj=longlat"))

currentEnv <- getData("worldclim", var="bio", res=2.5, path = "data/")

climList <- list.files(path = "data/wc2-5/", pattern = ".bil$", # getting the climate data from the folder
                       full.names = T) 

clim <- raster::stack(climList)

plot(clim[[12]])

plot(ranaDataSpatialPts, add = TRUE)

mask <- raster(clim[[1]])

geographicExtent <- extent(x = ranaDataSpatialPts)

set.seed(45)

backgroundPoints <- randomPoints(mask = mask, 
                                 n = nrow(ranaDataNotCoords),
                                 ext = geographicExtent, 
                                 extf = 1.25,
                                 warn = 0)

colnames(backgroundPoints) <- c("longitude", "latitude") # name columns

occEnv <- na.omit(raster::extract(x = clim, y = ranaDataNotCoords))
absenceEnv<- na.omit(raster::extract(x = clim, y = backgroundPoints))

presenceAbsenceV <- c(rep(1, nrow(occEnv)), rep(0, nrow(absenceEnv)))
presenceAbsenceEnvDf <- as.data.frame(rbind(occEnv, absenceEnv)) 

ranaSDM <- dismo::maxent(x = presenceAbsenceEnvDf, 
                         p = presenceAbsenceV,  
                         path=paste("output/maxent_outputs"))

predictExtent <- 1.25 * geographicExtent
geographicArea <- crop(clim, predictExtent, snap = "in")

ranaPredictPlot <- raster::predict(ranaSDM, geographicArea)

raster.spdf <- as(ranaPredictPlot, "SpatialPixelsDataFrame")
ranaPredictDf <- as.data.frame(raster.spdf)

wrld <- ggplot2::map_data("world")

xmax <- max(ranaPredictDf$x)
xmin <- min(ranaPredictDf$x)
ymax <- max(ranaPredictDf$y)
ymin <- min(ranaPredictDf$y)

dev.off()
ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = ranaPredictDf, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) + # expand = F fixes weird margin
  scale_size_area() +
  borders("state") +
  labs(title = "SDM of R. Boylii Under \nCurrent Climate Conditions",
       x = "longitude",
       y = "latitude",
       fill = "Environmental \nSuitability") + # \n is a line break
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5))

ggsave(filename = "CurrentSDM.jpg",
       plot=last_plot(),
       path="output", 
       width=1600, 
       height=800, 
       units="px") # save graph as a jpg file

# Future SDM

currentEnv <- clim # renaming from the last sdm

futureEnv <- raster::getData(name = 'CMIP5', var = 'bio', res = 2.5,
                             rcp = 45, model = 'IP', year = 70, path="data")

names(futureEnv) = names(currentEnv)

plot(currentEnv[[1]]) #current vs future climate
plot(futureEnv[[1]])

geographicAreaFutureC5 <- crop(futureEnv, predictExtent) #crop clim to desired map area

ranaPredictPlotFutureC5 <- raster::predict(ranaSDM, geographicAreaFutureC5)  #predict model onto future climate

raster.spdfFutureC5 <- as(ranaPredictPlotFutureC5, "SpatialPixelsDataFrame")
ranaPredictDfFutureC5 <- as.data.frame(raster.spdfFutureC5) # change prediction to data frame for gg plot

wrld <- ggplot2::map_data("world") #plot gg plot

xmax <- max(ranaPredictDfFutureC5$x)
xmin <- min(ranaPredictDfFutureC5$x)
ymax <- max(ranaPredictDfFutureC5$y)
ymin <- min(ranaPredictDfFutureC5$y) # limit map to desired latitude and longitude

ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = ranaPredictDfFutureC5, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
  scale_size_area() +
  borders("world") +
  borders("state") +
  labs(title = "SDM of R. Boylii Under \nCMIP 5 Climate Conditions",
       x = "longitude",
       y = "latitude",
       fill = "Env Suitability") +
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5))

ggsave(filename = "tempFutureSDM.jpg",
       plot=last_plot(),
       path="output", 
       width=1600, 
       height=800, 
       units="px") # save graph as a jpg file

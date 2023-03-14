install.packages("dismo")
install.packages("maptools")
install.packages("tidyverse")
install.packages("rJava")
install.packages("maps")
install.packages("geodata")

library(dismo)
library(maptools)
library(tidyverse)
library(rJava)
library(maps)
library(geodata)

#formatting
froglonglat<-cleanFrog%>% dplyr::select(longitude,latitude)
ranaDataSpatialPts <- SpatialPoints(froglonglat, proj4string = CRS("+proj=longlat"))

#Current enviornment data
curcurrentEnv <- getData("worldclim", var="bio", res=2.5, path="data") 
climList <- list.files(path = "data/wc2-5/", pattern = ".bil$", 
                       full.names = T)

#creating raster stack 
clim<-raster::stack(climList)
plot(clim[[12]])
plot(ranaDataSpatialPts, add=TRUE)

mask <- raster(clim[[1]]) 

#create shadow points to see where they could be
geographicExtent <- extent(x = ranaDataSpatialPts)


#create point where there aren't frogs to contrast the present points.
set.seed(45) 
backgroundPoints <- randomPoints(mask = mask, 
                                 n = nrow(froglonglat),
                                 ext = geographicExtent, 
                                 extf = 1.25, 
                                 warn = 0) 

colnames(backgroundPoints) <- c("longitude", "latitude")

#combine the points where we do have R. boylii and where we dont
occEnv <- na.omit(raster::extract(x = clim, y = froglonglat))
absenceEnv<- na.omit(raster::extract(x = clim, y = backgroundPoints))

#making a key that says where there are frog =1 and where there are no frgos=0
presenceAbsenceV <- c(rep(1, nrow(occEnv)), rep(0, nrow(absenceEnv)))
presenceAbsenceEnvDf <- as.data.frame(rbind(occEnv, absenceEnv)) 


#create a folder to store this output minre is hw6-output
ranaSDM <- dismo::maxent(x = presenceAbsenceEnvDf, 
                         p = presenceAbsenceV,  
                         path=paste("output/hw6-output"))



#crop climate data to match the occurence data
predictExtent <- 1.25 * geographicExtent 
geographicArea <- crop(clim, predictExtent, snap = "in") 

#stack the climate and occurrence data then turn into data frame for ggplot
ranaPredictPlot <- raster::predict(ranaSDM, geographicArea)  
raster.spdf <- as(ranaPredictPlot, "SpatialPixelsDataFrame")
ranaPredictDf <- as.data.frame(raster.spdf)

#add borders and map
wrld <- ggplot2::map_data("world")

#set range for the SDM by limiting the x andss y values
xmax <- max(ranaPredictDf$x)
xmin <- min(ranaPredictDf$x)
ymax <- max(ranaPredictDf$y)
ymin <- min(ranaPredictDf$y)

dev.off()

#
ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = ranaPredictDf, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F)+
  scale_size_area() +
  borders("state") +
  labs(title = "SDM of R. boylii under conditions in 2023",
       x = "longitude",
       y = "latitude",
       fill = "Environmental \nSuitability") + 
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5)) 

#save plot to file
ggsave("currentSDM.png", width=NA, scale=3)


#Start Future SDM
# current climate data was already loaded so I just start with sticking for future enviornment
futureEnv <- raster::getData(name = 'CMIP5', var = 'bio', res = 2.5,
                             rcp = 45, model = 'IP', year = 70, path="data") 

names(futureEnv) = names(curcurrentEnv)
# look at current vs future climate vars
plot(curcurrentEnv[[1]])
plot(futureEnv[[1]])



# crop map to use just the data in the area you want
geographicAreaFutureC5 <- crop(futureEnv, predictExtent)


#stack the environmental data and future occurrence data like we did when making the current SDM 
ranaPredictPlotFutureC5 <- raster::predict(ranaSDM, geographicAreaFutureC5)  

#turn the rastar stack into a data frame for formating purposes
raster.spdfFutureC5 <- as(ranaPredictPlotFutureC5, "SpatialPixelsDataFrame")
ranaPredictDfFutureC5 <- as.data.frame(raster.spdfFutureC5)


# set the range of the x and y axis so that the zoom of the graph is focused only on the areas where R. Boylii has been seen (not a map of the whole world)
xmax <- max(ranaPredictDfFutureC5$x)
xmin <- min(ranaPredictDfFutureC5$x)
ymax <- max(ranaPredictDfFutureC5$y)
ymin <- min(ranaPredictDfFutureC5$y)


ggplot() +
  geom_polygon(data = wrld, mapping = aes(x = long, y = lat, group = group),
               fill = "grey75") +
  geom_raster(data = ranaPredictDfFutureC5, aes(x = x, y = y, fill = layer)) + 
  scale_fill_gradientn(colors = terrain.colors(10, rev = T)) +
  coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = F) +
  scale_size_area() +
  borders("world") +
  borders("state") +
  labs(title = "SDM of R. boylii Under CMIP 5 Climate Conditions",
       x = "longitude",
       y = "latitude",
       fill = "Env Suitability") +
  theme(legend.box.background=element_rect(),legend.box.margin=margin(5,5,5,5)) 

ggsave("FutureSDM.png", width = NA, scale=3)


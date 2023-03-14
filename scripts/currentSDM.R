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

ggsave(filename = "currentSDM.jpg",
       plot=last_plot(),
       path="output", 
       width=1600, 
       height=800, 
       units="px") # save graph as a jpg file


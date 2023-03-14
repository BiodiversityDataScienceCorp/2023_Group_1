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

ggsave(filename = "futureSDM.jpg",
       plot=last_plot(),
       path="output", 
       width=1600, 
       height=800, 
       units="px") # save graph as a jpg file


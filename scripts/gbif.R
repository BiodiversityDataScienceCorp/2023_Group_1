
#Install Packages 
install.packages("spocc")
install.packages("tidyverse")
library(spocc)
library(tidyverse)
library(tidyverse)
#getting Data from gbif via my query for Rana boylii with a limit of 4000 occurences 
myQuery<-occ(query="Rana boylii", from="gbif", limit=4000)
myQuery

frog<-myQuery$gbif$data$Rana_boylii

#initial graphing of occurence data
ggplot()+
  geom_point(data=frog, mapping=aes(x=latitude, y= longitude),show.legend = FALSE)+
  labs(title="Data Occurence of R. Boylii")

#getting country map lines

wrld<-ggplot2::map_data("world")

#adding country lines

ggplot()+
  geom_polygon(data=wrld, mapping = aes(x=long, y=lat,group=group),fill="grey75", colour="grey66")+
  geom_point(data=frog, mapping=aes(x=longitude, y= latitude),show.legend = FALSE)+
  labs(title="Data Occurence of R. Boylii")


#remove Alaska points because they were from the 70s, NA, and duplicates points
cleanFrog<-frog %>% filter(latitude!="NA"|longitude!="NA")%>%filter(latitude<50)%>% mutate(location=paste(latitude,longitude,dateIdentified, sep="/"))%>%
  distinct(location,.keep_all = TRUE)
  
#x and y limits to cretae a range and zoom for the model
xmax<-max(cleanFrog$longitude)
xmin<-min(cleanFrog$longitude)
ymax<-max(cleanFrog$latitude)
ymin<-min(cleanFrog$latitude)

ggplot()+
  geom_polygon(data=wrld, mapping = aes(x=long, y=lat,group=group),fill="floralwhite", colour="grey60")+
  geom_point(data=cleanFrog, mapping=aes(x=longitude, y= latitude),show.legend = FALSE)+
  labs(title="Data Occurence of R. Boylii", x="Longitude", y = "Latitude")+
  coord_fixed(xlim=c(xmin,xmax), ylim=c(ymin,ymax))+
  scale_size_area()+
  borders("state")

ggsave(filename = "occurrenceMapRBoylii.jpg",
       plot=last_plot(),
       path="output", 
       width=1600, 
       height=800, 
       units="px") # save graph as a jpg file

       
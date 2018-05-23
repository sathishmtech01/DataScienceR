# Tutorials
# http://www.geo.ut .ee/aasa/LOOM02331/heatmap_in_R.html
# https://stackoverflow.com/questions/32148564/heatmap-plot-by-value-using-ggmap?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
# http://www.distancelatlong.com/country/india

# required packages
library(dplyr)
library(ggmap)
library(ggmap)
library(RColorBrewer)

# Fetching the data 
data1 = read.csv("heatmap.csv")
#data2 = data.frame(data1["Emp.Name"],data1["Level"])
colnames(data1)

# selected data
sel_data = as.data.frame(group_by(data1,Location..City.,Training.Identified) %>% summarize(n=n()))
sel_data["City"]=sel_data[ "Location..City."]

# Data : latitute and longitude of cites
lat_long_cites = read.csv("city_names.csv",sep = "\t")

# selected data for Indian map heat mapping
sel_data_heatmap = left_join(x=sel_data,y=lat_long_cites,by= "City" ) %>% select( "n","City","Training.Identified","Latitude","Longitude") %>% filter(Training.Identified =="Angular JS" )

# Map


#MyMap <- get_map(location =  as.numeric(geocode("India")), source = "google", maptype = "roadmap", crop = FALSE, zoom = 7)

MyMap <- ggmap(get_map(location =  as.numeric(geocode("India")), source = "google", maptype = "roadmap", crop = FALSE,zoom=5))

# Ref : https://www.r-bloggers.com/visualising-thefts-using-heatmaps-in-ggplot2/
# https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps/
# Displayy the heat map in Indian map
#MyMap + geom_tile(data = sel_data_heatmap, aes(x = Longitude, y = Latitude, alpha = n),size=2,
#                          fill = 'red') + theme(axis.title.y = element_text("Latitude"), axis.title.x = element_text("Longitude"))
YlOrBr <- c("#FFFFD4", "#FED98E", "#FE9929", "#D95F0E", "#993404")
MyMap %+% sel_data_heatmap +
  aes(x = Longitude, y = Latitude, z = n) +
  stat_summary2d(fun = median, binwidth = c(.5, .5), alpha = 0.5) +
  scale_fill_gradientn(name = "Median", colours = YlOrBr, space = "Lab") +
  labs(x = "Longitude", y = "Latitude") +
  coord_map()


 # ggplot(aes(x, y,fill=n)) + geom_tile()

####################################
##Slawa Rokicki
##Oct 26, 2015
##Harvard University
##Mapping with ggplot
####################################

library(ggplot2)
library(maptools)
library(rgeos)
library(Cairo)
library(ggmap)
library(scales)
library(RColorBrewer)
set.seed(8000)

##input shapefiles
setwd("/Users/slawarokicki/Dropbox/3ie Replication Project/Analysis/Shapefiles/")
states.shp <- readShapeSpatial("IND_adm/IND_adm1.shp")
class(states.shp)
names(states.shp)

##create (or input) data to plot on map
num.states<-length(states.shp$NAME_1)
mydata<-data.frame(NAME_1=states.shp$NAME_1, id=states.shp$ID_1, prevalence=rnorm(num.states, 35, 15))
head(mydata)

#fortify shape file to get into dataframe mode
states.shp.f <- fortify(states.shp, region = "ID_1")
class(states.shp.f)
head(states.shp.f)

#merge with coefficients and reorder
merge.shp.coef<-merge(states.shp.f, mydata, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ] 

#basic plot
ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill = prevalence), 
               color = "black", size = 0.25) + 
  coord_map()


#nicer plot
ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill = prevalence), 
               color = "black", size = 0.25) + 
  coord_map()+
  scale_fill_distiller(name="Percent", palette = "YlGn", breaks = pretty_breaks(n = 5))+
  theme_nothing(legend = TRUE)+
  labs(title="Prevalence of X in India")

#aggregate data to get mean latitude and mean longitude for each state
cnames <- aggregate(cbind(long, lat) ~ NAME_1, data=final.plot, FUN=function(x) mean(range(x)))

#plot using geom_text
ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill = prevalence), 
               color = "black", size = 0.25) + 
  coord_map() +
  scale_fill_gradient(name="Percent", limits=c(0,70), low="white", high="red")+
  theme_nothing(legend = TRUE)+
  labs(title="Prevalence of X in India")+
  geom_text(data=cnames, aes(long, lat, label = NAME_1), size=3, fontface="bold")

#save map
p2 <- ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill = prevalence), 
               color = "black", size = 0.25) + 
  coord_map()+
  scale_fill_distiller(name="Percent", palette = "YlGn", breaks = pretty_breaks(n = 4))+
  theme_nothing(legend = TRUE)+
  labs(title="Prevalence of X in India")

ggsave(p2, file = "map1.png", width = 6, height = 4.5, type = "cairo-png")

#or save using pdf function
pdf("mymap.pdf")
print(p2)
dev.off()
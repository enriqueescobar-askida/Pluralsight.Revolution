# PA Unconventional Drilled Wells (1-1-2000 through 7-29-2013)
# Unzip, extract and load a csv

url<- 'https://s3-us-west-2.amazonaws.com/downloads.fractracker.org/PA/PA_UncDWs_07292013.zip'
download.file(url, 'tmp.zip', method='curl', mode = "wb")
df <- read.csv(unz('tmp.zip','PA_UncDWs_07292013.csv'))

## Maps Package
library(maps)
map('state','pennsylvania')
points(df$LONGITUDE, df$LATITUDE, cex=0.1)

## Map Projections Package
library(mapproj)

map('state')
map('state', proj='azequidistant')
map('state', proj='orthographic')
map('state', proj='bonne', param=65)

map()
map(proj='orthographic')

## GGMAP
library(ggmap)
ggmap(get_map())
m <- ggmap(get_map(location='pennsylvania',zoom=7, maptype = 'roadmap'))
m + geom_point(data=df, aes(x=LONGITUDE, y=LATITUDE, color=WELL_STATUS))



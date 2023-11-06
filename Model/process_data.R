library(sf)
library(raster)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(stars)

#import all rasters from the Data folder 
rastlist <- list.files(path = "Data/Population/", pattern='.tif$', all.files= T, full.names= T)
cities_bbbox <- read.csv("Data/city-areas.csv")

country.list <- list()

for (i in 1: length(rastlist)-1){
# read
country.sf <- read_stars(rastlist[[i]]) 
country.sf <- country.sf %>%
        mutate(orig_name = names(country.sf)[1])
# crop
city <- cities_bbbox %>%
  filter(file_name == names(country.sf)[1])

bb <- st_bbox(c(xmin = city$long_min,
                ymin = city$lat_min,
                xmax = city$long_max,
                ymax = city$lat_max), crs = st_crs(country.sf))

country.crop <- st_crop(country.sf, bb)

if (names(country.sf)[1] == "sgp_pd_2020_1km_UNadj.tif"){
  
}
country.crop <- country.crop %>%
  st_as_sf() %>%
  mutate(city = city$city)  %>%
  as.data.frame() %>%
  rename_at(1, ~"Pop_density" )

st_write(country.crop, paste0("Data/Pop_density_",city$city,".shp"))
}

## Singapore no need to crop 
i=10 

# read
country.sf <- read_stars(rastlist[[i]]) 
country.sf <- country.sf %>%
  mutate(orig_name = names(country.sf)[1])
# crop
city <- cities_bbbox %>%
  filter(file_name == names(country.sf)[1])

country.sf <- country.sf %>%
  st_as_sf() %>%
  mutate(city = city$city)  %>%
  as.data.frame() %>%
  rename_at(1, ~"Pop_density" )

st_write(country.sf, paste0("Data/Pop_density_",city$city,".shp"))

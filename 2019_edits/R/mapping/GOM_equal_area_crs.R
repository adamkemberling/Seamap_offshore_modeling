#### Coordinate Reference System for GOM Equal Area  ####

library(sf)
library(tidyverse)

# Working with custom CRS for Gulf of Mexico


# found a resource fo4r selecting a crs for gulf oil spill work, pretty relevant to this work*
# for more details on naming crs objects go here https://proj4.org/operations/projections/aea.html
gulf_crs <- "+proj=aea +lat_1=23.000000 +lat_2=28.000000 +x_0=1200000.000000 +units=km"
# proj=aea : projection equals albers equal area
# lat_1    : first standard parallel = 23N
# lat_2    : second standard parallel = 28N
# x_0      : false easting
# units    : units = km


#Load a wgs1984 geographic coordinate system shapefile
usa_poly <- usa_adam <- read_sf("Y:/GIS Files/GIS shapefiles/Gulf of Mexico/gom_states.shp") %>% 
  st_set_crs(4326)



# Transform to our albers equal area projection
usa_poly_km <- st_transform(usa_poly, gulf_crs)

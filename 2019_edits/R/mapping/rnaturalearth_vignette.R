

#R Natural Earth Data Mapping
#From : https://cran.r-project.org/web/packages/rnaturalearth/vignettes/rnaturalearth.html

#install.packages("rnaturalearth")
#install.packages(rnaturalearth)

#load necessary packages
library(rnaturalearth)
library(sp)
library(sf)
library(tidyverse)


# world at small scale (low resolution)
sp::plot(ne_countries(type = 'countries', scale = 'small'))


# countries, UK undivided
sp::plot(ne_countries(country = 'united kingdom', type='countries'))

# map_units, UK divided into England, Scotland, Wales and Northern Ireland
sp::plot(ne_countries(country = 'united kingdom', type='map_units'))

# countries, small scale
sp::plot(ne_countries(country = 'united kingdom', scale = 'small'))   

# countries, medium scale
sp::plot(ne_countries(country = 'united kingdom', scale = 'medium'))

# countries, large scale
sp::plot(ne_countries(country = 'united kingdom', scale = 'large'))

# states country='united kingdom'
sp::plot(ne_states(country = 'united kingdom'))  
# states geounit='england'
sp::plot(ne_states(geounit = 'england')) 

# states country='france'
sp::plot(ne_states(country = 'france'))


# coastline of the world
# subsetting of coastline is not possible because the Natural Earth data are not attributed in that way
sp::plot(ne_coastline())




####  Applying the vignette to Gulf of Mexico Study area  ####

#Loading them in stores them as SpatialPolygonsDataFrame
usa <- ne_states(country = "united states of america")
plot(usa)

#convert to sf
usa_sf <- st_as_sf(usa)
ggplot() +
  geom_sf(data = usa_sf, aes(fill = region))

#Pull out Southern States
us_south <- usa_sf %>% filter(region == "South")
ggplot() +
  geom_sf(data = us_south, aes(fill = name), show.legend = FALSE)


#Grab Mexico
mexico <- ne_states(country = "mexico")
plot(mexico)

#convert to sf
mex_sf <- st_as_sf(mexico)
ggplot() +
  geom_sf(data = mex_sf, aes(fill = name), show.legend = FALSE)


#Plot both together and add scales
library(ggspatial)
ggplot() +
  #geom_rect(aes(xmin = -100, xmax = -79.5, ymin = 18.5, ymax = 31.5), fill = "skyblue") +
  geom_sf(data = us_south, fill = "antiquewhite1") +
  geom_sf(data = mex_sf, fill = "antiquewhite1") +
  coord_sf(xlim = c(-100, -79.5), ylim = c(18.5, 31.5), expand = FALSE) +
  annotation_scale(location = "br", width_hint = 0.5) + 
  annotation_north_arrow(location = "br", 
                         which_north = "true", 
                         pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.5, "in"), 
                         style = north_arrow_fancy_orienteering) +
  theme(panel.grid = element_line(linetype = 2, color = gray(.5), size = 0.25),
        panel.background = element_rect(fill = "skyblue"))

#Not bad!



# How its is set up for mapping shiny app
#Use rnaturalearth data instead of loading big shapefiles
#US Coast and State Borders shapefile
usa <- ne_states(country = "united states of america") %>% 
  st_as_sf() %>% 
  filter(region == "South")

#Grab Mexico
mexico <- ne_states(country = "mexico") %>%  
  st_as_sf()


# Make the plot 
ggplot() +
  # Landmasses and Ocean color
  geom_rect(aes(xmin = -98, xmax = -80, ymin = 23.5, ymax = 31.5), fill = "aliceblue") +
  geom_sf(data = mexico, fill = "antiquewhite1") +
  geom_sf(data = usa, fill = "antiquewhite1") +
  coord_sf(xlim = c(-97.99, -80.001), ylim = c(23.5, 31.5), expand = FALSE) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Generic Gulf of Mexico Plot",
    caption = "rnaturalearth package"
  ) 

# Aggregating Tagging Coverage for NOAA final report Maps
# 10/17/2019


#Packages
library(tidyverse)
library(here)
library(sf)
library(rnaturalearth)
library(gridExtra)

#Data
tagging <- read_csv(here("data", "noaa_report", "tags_clean_final.csv"))
recaps <- read_csv(here("data", "noaa_report", "tags_recaps_clean_final.csv"))
usa_poly <- ne_states("united states of america") %>% st_as_sf() %>% filter(region == "South")
mexico_poly <- ne_states("mexico") %>% st_as_sf()

#Reduce resolution and then group by lat/long coordinates, count the rows for each for number tagged
tag_bins <- tagging %>% 
  mutate(long_decdeg = round(long_decdeg, 2),
         lat_decdeg  = round(lat_decdeg, 2)) %>%
  group_by(long_decdeg, lat_decdeg) %>% 
  summarise(n_tagged = n()) %>% 
  st_as_sf(coords = c("long_decdeg", "lat_decdeg"), crs = 4326)


ggplot() +
  geom_sf(data = usa_poly) +
  geom_sf(data = tag_bins, aes(size = n_tagged), color = "royalblue", fill = "royalblue", alpha = 0.6) + 
  coord_sf(xlim = c(-97.5, -82.5), ylim = c(25.8, 30.8)) +
  theme_bw() + 
  ggtitle("Rounded to 2-digit(s)")


#Will want to do custom bins and shape sizes and add them to the dataframe  



#Side-by-side comparison
no_agg <- tagging %>% 
    filter(between(long_decdeg, left = -91.2, right = -89)) %>% 
    st_as_sf(coords = c("long_decdeg", "lat_decdeg"), crs = 4326) %>% 
    ggplot() +
    geom_sf(data = usa_poly) +
    geom_sf(color = "royalblue", fill = "royalblue", alpha = 0.6) + 
    coord_sf(xlim = c(-91.2, -89), ylim = c(29, 30.5)) +
    theme_bw() + theme(legend.position = "bottom") +
    ggtitle("Original Data")



agg_plot <- ggplot() +
  geom_sf(data = usa_poly) +
  geom_sf(data = tag_bins, aes(size = n_tagged), color = "royalblue", fill = "royalblue", alpha = 0.6, show.legend = FALSE) + 
  coord_sf(xlim = c(-91.2, -89), ylim = c(29, 30.5)) +
  theme_bw() + theme(legend.position = "bottom") +
  ggtitle("Rounded to 100th of a Degree")

agg_plot

grid.arrange(grobs = list(no_agg, agg_plot), nrow = 1)  





####  Tagging Areas  ####
s_texas <- ggplot() +
  geom_sf(data = usa_poly) +
  geom_sf(data = mexico_poly) +
  geom_sf(data = tag_bins, aes(size = n_tagged), alpha = 0.8, show.legend = FALSE) + 
  coord_sf(xlim = c(-97.9, -96.4), ylim = c(26, 27.6)) +
  theme_bw() + 
  ggtitle("South Texas")
s_texas

mid_texas <- ggplot() +
  geom_sf(data = usa_poly) +
  geom_sf(data = tag_bins, aes(size = n_tagged), alpha = 0.8, show.legend = FALSE) + 
  coord_sf(xlim = c(-97.5, -96), ylim = c(27.6, 28.8)) +
  theme_bw() + 
  ggtitle("Mid-Texas")
mid_texas

north_texas <- ggplot() +
  geom_sf(data = usa_poly) +
  geom_sf(data = tag_bins, aes(size = n_tagged), alpha = 0.8, show.legend = FALSE) + 
  coord_sf(xlim = c(-96, -94), ylim = c(28.2, 30.2)) +
  theme_bw() + 
  ggtitle("North Texas")
north_texas

#Better Shapefile
usa_coastline <- rgdal::readOGR()

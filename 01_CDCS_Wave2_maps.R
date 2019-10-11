# Request: Create custom maps of CDCS wave 2 countries
# Author: Tim Essam, GeoCenter
# Date: 2019_10_04
# Notes: List of countries for inclusion included in email
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

# Notes: package load order may vary depending on when this file is executed. Maps package naturally masks the map function from purrr so be aware of loading order!
# 
# library(ggmap)
# library(tidyverse)
# library(maps)
library(sf)
library(rgeos)
library(llamar)
library(extrafont)

library("rnaturalearth")
library("rnaturalearthdata")





# Prep world polygons as a sf object to filter and map --------------------
world <- sf::st_as_sf(map('world2', plot = FALSE, fill = TRUE)) 

world <- ne_countries(scale = "large", returnclass = "sf")
class(world)
names(world)


world %>% 
  #filter(str_detect(sovereignt, "Tanz")) %>% 
  count(sovereignt) %>% 
  print(n = Inf)


# List of provided countries for the Indo-Pacific Map
cdcs <- c("Ghana", "Egypt", "Honduras", "Indonesia", "Senegal", "East Timor", "Moldova", 
          "Armenia", "Kenya", "Madagascar", "Nigeria", "United Republic of Tanzania", "Kyrgyzstan", "Morocco", "Dominican Republic", "Mali")

cdcs_GC <- c("Uganda", "Malawi", "Kenya", "Georgia")


cdcs_map <- world %>% 
  filter(sovereignt %in% cdcs) %>% 
  mutate(sovereignt = ifelse(sovereignt == "United Republic of Tanzania", "Tanzania", sovereignt))

cdcs_GC_map <- world %>% 
  filter(sovereignt %in% cdcs_GC) 

cdcs_map %>% count(sovereignt) 

# Bounding box
cdcs_map %>% st_bbox()  %>% st_crs()


crs <- "+proj=latlong +datum=WGS84 +pm=madrid"

custom_map <- function(df, title = "placeholder") {
  ggplot() +
    geom_sf(data = world, 
            fill = grey10K, colour = "#FFFFFF", size = 0.1) +
    geom_sf(data = df, fill = grey30K, colour = "#FFFFFF", size = 0.1) +
    geom_sf_text(data = df, aes(label = sovereignt),
                  check_overlap = TRUE,
                  family = "Lato Light",
                  colour = grey90K) +
    coord_sf(xlim = c(-100, 150),
             ylim = c(-35, 65)) +
    labs(x = "", y = "") +
    #coord_sf(crs = crs) +
    theme_basic() +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    labs(title = title)
}

cdcs_draft <- custom_map(cdcs_map, title = "CDCS Wave 2 Countries")  
custom_map(cdcs_GC_map) + coord_sf(xlim = c(-0, 60),
                                            ylim = c(-20, 50))
cdcs_draft

ggsave(file.path(graphpath, "CDCS_wave2_countries.png"),
       plot = cdcs_draft,
       device = "png",
       width = 11.5, 
       height = 8,
       units = "in",
       dpi = "retina")


# GIS Specialists coverage ------------------------------------------------

gis_spec <- c("Haiti", "Honduras",
              "Colombia", "Senegal",
              "Mali", "Liberia",
              "Ivory Coast", "Ghana",
              "Nigeria", "Democratic Republic of the Congo", 
              "Uganda", "Kenya",
              "Malawi", "Mozambique",
              "South Africa", "Madagascar",
              "Georgia", "West Bank",
              "Lebanon", "Egypt",
              "Jordan", "Afghanistan",
              "Pakistan", "Nepal", "Bangladesh",
              "Indonesia", "United States of America")


gis_spec_map <- world %>% 
  filter(sovereignt %in% gis_spec) 

gis_spec_map_graph <- custom_map(gis_spec_map)

ggsave(file.path(graphpath, "GIS_specialists_network.pdf"),
       plot = gis_spec_map_graph,
       device = "pdf",
       width = 11.5, 
       height = 8,
       units = "in",
       dpi = "retina",
       useDingbats = FALSE)


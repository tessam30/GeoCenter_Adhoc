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
# library(sf)
# library(rgeos)
# library(llamar)
# library(extrafont)



# Prep world polygons as a sf object to filter and map --------------------
world <- sf::st_as_sf(map('world2', plot = FALSE, fill = TRUE)) 


world %>% 
  count(ID) %>% 
  print(n = Inf)

# Initially, these stubs did not show up; Tuvalu appears to be missing from dataset
world %>% 
  filter(grepl(c("Lao|Marshall|Solom|Tuvalu|Vanu"), ID)) %>% 
  count(ID)

# List of provided countries for the Indo-Pacific Map
cdcs <- c("Ghana", "Egypt", "Honduras", "Indonesia", "Senegal", "Timor-Leste", "Moldova", 
          "Armenia", "Kenya", "Madagascar", "Nigeria", "Tanzania", "Kyrgyzstan", "Morocco", "Dominican Republic", "Mali")


cdcs_map <- world %>% 
  filter(ID %in% cdcs) 

cdcs_map %>% count(ID)



custom_map <- function(df, title = "placeholder") {
  ggplot() +
    geom_sf(data = world, 
            fill = grey20K, colour = "#FFFFFF", size = 0.1) +
    geom_sf(data = df, fill = grey40K, colour = "#FFFFFF", size = 0.1) +
    geom_sf_text(data = df, aes(label = ID),
                 check_overlap = TRUE,
                 family = "Lato Light",
                 colour = grey90K) +
    #coord_sf(xlim = c(-150, 150),
     #       ylim = c(-55, 55)) +
    #theme_basic() +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    labs(title = title)
}

cdcs_draft <- custom_map(cdcs_map, title = "CDCS Wave 2 Countries")  
cdcs_draft

ggplot(cdcs_map) + geom_sf(data = world)

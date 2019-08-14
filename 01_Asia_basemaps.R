# Request: Create custom maps for Asia Bureau
# Author: Tim Essam, GeoCenter
# Date: 2019_08_14
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
indo_pacific <- c( 'Myanmar', 'Cambodia', 'Indonesia', 'Mongolia', 'Laos', 'Micronesia', 'Fiji', 'Kiribati', 'Marshall Islands', 'Nauru', 'Palau', 'Papua New Guinea', 'Samoa', 'Solomon Islands', 'Tuvalu', 'Vanuatu', 'Philippines', 'Thailand', 'Timor-Leste', 'Vietnam', 'Bangladesh', 'India', 'Maldives', 'Nepal', 'Sri Lanka')

indo_map <- world %>% 
  filter(ID %in% indo_pacific) 

indo_map %>% count(ID)

# List of countries provided for the USAID Asia Region Map
asia_region <- c("Myanmar", "Cambodia", "China", "Indonesia", "Mongolia", "Laos", "Micronesia", "Fiji", "Kiribati", "Marshall Islands", "Nauru", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu", "Philippines", "Thailand", "Timor-Leste", "Vietnam", "Bangladesh", "India", "Maldives", "Nepal", "Kazakhstan", "Kyrgyzstan", "Turkmenistan", "Uzbekistan", "Tajikistan", "Bhutan")

asia_map <- 
  world %>% 
  filter(ID %in% asia_region)

asia_map %>% count(ID) %>% print(n = Inf)



# Map with poloygons - 
#coord_sf(crs = "+proj=laea +lat_0=4.4 +lon_0=133.2")


# Reusable function to create the maps based on filtered sf object --------

custom_map <- function(df, title = "placeholder") {
  ggplot() +
    geom_sf(data = world, 
            fill = grey20K, colour = "#FFFFFF", size = 0.1) +
    geom_sf(data = df, fill = grey40K, colour = "#FFFFFF", size = 0.1) +
    geom_sf_text(data = df, aes(label = ID),
                 check_overlap = TRUE,
                 family = "Lato Light",
                 colour = grey90K) +
    coord_sf(xlim = c(40, 200),
             ylim = c(-25, 55)) +
    theme_basic() +
    theme(panel.background = element_rect(fill = "aliceblue")) +
    labs(title = title)
}

indo_pac <- custom_map(indo_map, title = "Indo-Pacific Countries")  
asia_reg <- custom_map(asia_map, title = "USAID Asia Region")


# Write the products to pdf or svg depending on need ----------------------

ggsave(file.path(graphpath, "Indo-Pacific Countries.pdf"),
       plot = indo_pac,
       width = 11.5, 
       height = 8,
       units = "in",
       dpi = "retina",
       useDingbats = FALSE)

ggsave(file.path(graphpath, "USAID Asia Region.pdf"),
       plot = asia_reg,
       width = 11.5, 
       height = 8,
       units = "in",
       dpi = "retina",
       useDingbats = FALSE)







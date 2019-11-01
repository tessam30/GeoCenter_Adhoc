# Request: Investigate Entrepreneurship data for plotting
# Author: Tim Essam, GeoCenter
# Date: 2019_11_01
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
library(tidytext)
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


cdcs_map <- world %>% 
  filter(sovereignt %in% cdcs) %>% 
  mutate(sovereignt = ifelse(sovereignt == "United Republic of Tanzania", "Tanzania", sovereignt))

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


# Entrepreneurship data ---------------------------------------------------

country_list <- c("Madagascar", "Laos", "Cambodia", "Peru", "Honduras", "Nicaragua",
                  "Georgia", "Azerbaijan", "Yemen", "Egypt")

# Sets colors of selected countries from above
color_vect <- c("0" = grey30K, "1" = grey70K)

df <- read_csv(file.path(datapath, "WE3_dashboard_2019.csv")) %>% 
  # filter(`Sub-Dimension` == "Entrepreneurship") %>% 
  mutate(country_flag = ifelse(Country %in% country_list, 1, 0), 
    indicator_sort = fct_reorder(`Indicator Name`, `Indicator Value`, .desc = TRUE),
        country_sort = reorder_within(Country, `Indicator Value`, `Indicator Name`)) 


# Function to create sorted bar plots
bar_sort <- function(df, x, y, fillvar, facetvar) {
  df %>% 
    ggplot(aes(x = {{ x }}, y = {{ y }}, fill = {{fillvar}})) +
    geom_col() +
    coord_flip() +
    facet_wrap(vars({{facetvar}}), scales = "free_y") +
    scale_x_reordered() +
    theme_xgrid() +
    scale_fill_manual(values = color_vect)
}


# Plot all the sub-indicator values
bar_sort(df, country_sort, `Indicator Value`, fillvar = as.character(country_flag), 
         facetvar = indicator_sort)


# Plot the 
df %>% group_by(Country, Region, country_flag, `Income Group`) %>% 
  summarise(value = mean(`Sub-Dimension Value`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(value)) %>% 
  filter(!Region %in% c("Oceania", "North America")) %>% 
  mutate(region_sort = fct_reorder(Region, value, .desc = TRUE),
         country_sort = reorder_within(Country, value, Region)) %>% 
  bar_sort(., country_sort, y = value, fill = as.character(country_flag),
           facetvar = region_sort) +
  labs(title = "Entrepreneurship")




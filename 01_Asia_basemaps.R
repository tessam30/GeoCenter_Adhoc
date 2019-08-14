# Notes: package load order may vary depending on when this file is executed. Maps package naturally masks the map function from purrr so be aware of loading order!

library(ggmap)
library(tidyverse)
library(maps)
library(sf)
library(rgeos)
library(llamar)
library(extrafont)

world <- sf::st_as_sf(map('world2', plot = FALSE, fill = TRUE)) 

world %>% count(ID) %>% print(n = Inf)

# Initially, these stubs did not show up; Tuvalu appears to be missing from dataset

world %>% 
  filter(grepl(c("Lao|Marshall|Solom|Tuvalu|Vanu"), ID)) %>% 
  count(ID)

indo_pacific <- c( 'Myanmar', 'Cambodia', 'Indonesia', 'Mongolia', 'Laos', 'Micronesia', 'Fiji', 'Kiribati', 'Marshall Islands', 'Nauru', 'Palau', 'Papua New Guinea', 'Samoa', 'Solomon Islands', 'Tuvalu', 'Vanuatu', 'Philippines', 'Thailand', 'Timor-Leste', 'Vietnam', 'Bangladesh', 'India', 'Maldives', 'Nepal', 'Sri Lanka')

indo_map <- world %>% 
  filter(ID %in% indo_pacific) 
indo_map %>% count(ID)


asia_region <- c("Myanmar", "Cambodia", "China", "Indonesia", "Mongolia", "Laos", "Micronesia", "Fiji", "Kiribati", "Marshall Islands", "Nauru", "Palau", "Papua New Guinea", "Samoa", "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu", "Philippines", "Thailand", "Timor-Leste", "Vietnam", "Bangladesh", "India", "Maldives", "Nepal", "Kazakhstan", "Kyrgyzstan", "Turkmenistan", "Uzbekistan", "Tajikistan", "Bhutan")

asia_map <- 
  world %>% 
  filter(ID %in% asia_region)

asia_map %>% count(ID) %>% print(n = Inf)




# Map with poloygons - Cost
ggplot() +
  geom_sf(data = world, 
          fill = grey20K, colour = "#FFFFFF", size = 0.1) +
  geom_sf(data = indo_map, fill = grey50K, colour = "#FFFFFF", size = 0.1) +
  geom_sf_text(data = indo_map, aes(label = ID),
               check_overlap = TRUE,
               family = "Lato Light",
               colour = grey90K) +
  coord_sf(xlim = c(40, 200),
           ylim = c(-25, 60)) +
  theme_blank()

custom_map <- function(df, title = "placeholder") {
  ggplot() +
    geom_sf(data = world, 
            fill = grey20K, colour = "#FFFFFF", size = 0.1) +
    geom_sf(data = df, fill = grey50K, colour = "#FFFFFF", size = 0.1) +
    geom_sf_text(data = df, aes(label = ID),
                 check_overlap = TRUE,
                 family = "Lato Light",
                 colour = grey90K) +
    coord_sf(xlim = c(40, 200),
             ylim = c(-25, 60)) +
    theme_basic() +
    labs(title = title)
}

indo_pac <- custom_map(indo_map, title = "Indo-Pacific Countries")  
asia_reg <- custom_map(asia_map, title = "USAID Asia Region")

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







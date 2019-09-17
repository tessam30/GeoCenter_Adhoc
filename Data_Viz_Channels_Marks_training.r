library(tidyverse)
library(llamar)

# Script to produce graphics for data viz training

df <- tibble::tribble(
        ~y, ~x, ~size, ~color,  ~class, ~y1,
        20,  2,     2,  "red", "alpha",  10,
        14,  6,     14, "blue",  "beta",  15,
         6, 10,     2,  "red", "gamma",   4
        )


# GGplot theme options for only showing x / y line
theme_xyline <- theme(axis.line = element_line(), 
                             axis.text.x=element_blank(),
                             axis.text.y=element_blank(),
                             axis.ticks=element_blank(),
                             axis.title.x=element_blank(),
                             axis.title.y=element_blank(),
                             legend.position="none",
                             panel.background=element_blank(),
                             panel.border=element_blank(),
                             panel.grid.major=element_blank(),
                             panel.grid.minor=element_blank(),
                             plot.background=element_blank())


# Mark = line, channel = vertical position
df %>% 
  ggplot(aes(x = class, y = y)) + geom_col() + llamar::theme_blank()

# Mark = point, channel = vertical and horizontal position
df %>% 
  ggplot(aes(x = x, y = y1)) + geom_point(size = 18) +
  scale_x_continuous(limits = c(0, 14)) + 
  theme_xyline

# Mark = point, channel = vertical and horizontal + color
df %>% 
  ggplot(aes(x = x, y = y1, colour = color)) + geom_point(size = 18) +
  scale_x_continuous(limits = c(0, 14)) + 
  theme_xyline

# Mark = point, channel = vertical and horizontal + color
df %>% 
  ggplot(aes(x = x, y = y1, colour = color, size = size)) + geom_point() +
  scale_x_continuous(limits = c(0, 14)) + 
  theme_xyline + scale_size_area(max_size = 30)
  

# Import Excel data for PMI, reshape and plot as bars
# Date: 2018_10_01
# Authore: Tim Essam
# Audience: GeoCenter


# Load data ---------------------------------------------------------------

# What is loadedin the session?
sessionInfo()

df <- read_excel(file.path(datapath, "GHANA_IR_Updated.xlsx"), sheet = "GH_ITN_Resistance")
colors <- read_excel(file.path(datapath, "GHANA_IR_Updated.xlsx"), sheet = "Colors_r")

# Convert the color palette to hex and use crosswalk to map colors
colors <- 
  colors %>%  
  mutate(hex = rgb(red = Red, green = Green, blue = Blue, maxColorValue = 255),
         hex = sQuote(hex)) 


df_long <- 
  df %>% 
  gather(., MinMort:PE_PBOControl, key = "drug", value = "effectiveness") %>% 
  left_join(., colors, by = c("drug" = "Drug"))

# Set the colors for each category



df_long %>% 
  ggplot(., aes(x = ADM2, y = effectiveness)) +
  geom_col(aes(fill = hex)) + 
  facet_wrap(~drug) +
  coord_flip()

  
# Purpose: Load FFP data, geocode and look into creating x --> y flow maps
# Author: Tim Essam, Ph.D.
# Date: 2018_12_12
# Notes: Quick turn around request


library(readxl)
library(ggmap)
library(lubridate)
library(zoo)
library(llamar)
library(gganimate)

source("ffp_shipping_ports.R")

df  <- read_excel(file.path(datapath, "FY18 Shipment Report DATA.xlsx"), sheet = "FY 2018")
ls.str(df)

# Check if list of ports and countries makes sense
df %>% 
  group_by(`Dicharge Port Country`, `Dicharge Port Name`) %>% 
  count() %>% print(n = Inf)


# Geocode data using
df_geo <- df  %>% 
  rename(discharge_port = `Dicharge Port Name`, 
         discharge_country = `Dicharge Port Country`) %>% 
  mutate(market_loc = paste(discharge_port, discharge_country, sep = ", "))

# --- Cannot use Google API calls with ggmap anymore. Somthing about google changing terms on API # # # calls. So, we use https://geocode.localfocus.nl on the cut and pasted call below. Strip out 
# markdown if needed
df_geo %>% group_by(market_loc) %>% count() %>% knitr::kable()

# Now, we shoudl be able to merge on the discharge_port and market
# Add in country names so you can do crosswalk
ffp_ports <- 
  df_geo %>% 
  left_join(., market_geo, by = c("discharge_port" = "market")) %>% 
  cbind(., orig_port) %>% 
  select(-lat, -lon) %>% 
  mutate(month = month(Date), 
         year= year(Date),
         month_year = as.yearmon(Date),
         month_date = as.Date.yearmon(month_year),
         country = case_when(
         discharge_country == "CONGO-REPUB. OF" ~ "Republic of Congo",
         discharge_country == "CONGO-DEM. REPUB." ~ "Democratic Republic of the Congo",
         TRUE ~ discharge_country))

# Summarize things down to the monthly level for each port
ffp_ports_monthly <- 
  ffp_ports %>% 
  group_by(discharge_port, month_year, 
           month_date,
           `Discharge Port Latitude`, 
           `Discharge Port Longitude`, 
           orig_lon, 
           orig_lat, 
           discharge_country, 
           `Geographic Region`) %>% 
  summarise(tot_costs = sum(`Total Freight Cost (Gross)`))


ffp_ports_fy <- 
  ffp_ports %>% 
  group_by(`Geographic Region`, country) %>%
  summarise(metric_tons = sum(`Metric Tons`),
            ocean_cost = sum(`Ocean Cost`)) %>% 
  left_join(., world, by = c("country" = "region"))

# Keep just the port of entry
ffp_ports_port <- 
  ffp_ports %>% 
  group_by(discharge_port, `Discharge Port Longitude`, `Discharge Port Latitude`, 
           orig_lat, orig_lon, discharge_country, country) %>%
  summarise(metric_tons_ports = sum(`Metric Tons`))




# Basic graph, with country
ffp_ports %>% 
  group_by(`Geographic Region`, country) %>%
  summarise(metric_tons = sum(`Metric Tons`),
            ocean_cost = sum(`Ocean Cost`)) %>% 
  ungroup() %>% 
  mutate(country_sort = as.factor(country), 
         geo_sort_tons = fct_reorder(country_sort, metric_tons), 
          geo_sort_cost = fct_reorder(country_sort, ocean_cost)) %>% 
  ggplot() +
  geom_col(aes(x = geo_sort_cost, y = ocean_cost)) +
  coord_flip()


# Map with poloygons - Cost
ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "#bababa", colour = "#FFFFFF", size = 0.1)+
  geom_polygon(data = ffp_ports_fy, aes(long, lat, fill = log(ocean_cost), group = group), 
               colour = "#1a1a1a", size = 0.25, alpha = 0.7)+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'Greens'),
                                               labels = scales::number) +
  coord_map("gilbert", xlim = c(-125, 125),
            ylim = c(-50, 50))

# Map with polygons - Shipping volume
ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "#bababa", colour = "#FFFFFF", size = 0.1)+
  geom_polygon(data = ffp_ports_fy, aes(long, lat, fill = log(metric_tons), group = group), 
               colour = "#1a1a1a", size = 0.25, alpha = 0.7)+
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, 'OrRd'),
                      labels = scales::number) +
  coord_map("gilbert", xlim = c(-125, 125),
            ylim = c(-50, 50)) +
  geom_point(data = ffp_ports_port, 
             aes(x = `Discharge Port Longitude`, 
                 y = `Discharge Port Latitude`,
                 size = (metric_tons_ports/10000)), 
             color = "#1a1a1a", 
             alpha = 0.9,
             shape=21, 
             stroke = 1)


  
# Month maps with facets
tmp <- ggplot(ffp_ports_monthly) +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "#bababa") + 
  geom_curve(data = ffp_ports_monthly, aes(x = orig_lon,
                 y = orig_lat,
                 xend = `Discharge Port Longitude`, 
                 yend = `Discharge Port Latitude`),
                color = "#878787",
             curvature = -0.25) + 
  coord_quickmap(xlim = c(-125, 125),
                 ylim = c(-60, 60)) +
  geom_point(data = ffp_ports_monthly,
             aes(`Discharge Port Longitude`, `Discharge Port Latitude`,
                 colour = `Geographic Region`) , size = 1) +
  facet_wrap(~month_year) +
  theme(legend.position = "top") +
  labs(title = 'month_year: {frame_time}') +
  transition_time(month_year)

animate(tmp, nframes = 11, fps = 2)




# Map the world + all data
world <- map_data("world")
ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "grey") + 
  geom_point(data = ffp_ports, aes(`Discharge Port Longitude`, 
                                   `Discharge Port Latitude`, 
             colour = as.factor(discharge_country)), size = 1) + 
  coord_cartesian() +# add in flows
  geom_curve(data = ffp_ports, aes(x = orig_lon,
                 y = orig_lat,
                 xend = `Discharge Port Longitude`, 
                 yend = `Discharge Port Latitude`), 
             curvature = 0.1)


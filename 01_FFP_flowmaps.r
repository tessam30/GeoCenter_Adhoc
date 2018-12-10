setwd("C:/Users/tessam/Downloads")
dir()
library(readxl)
library(ggmap)

install.packages("readxl")
library(readxl)
library(ggmap)
library(tidyverse)

df  <- read_excel("FY18 Shipment Report DATA.xlsx", sheet = "FY 2018")
View(df)

# Geocode data using
df_geo <- df  %>% rename(discharge_port = `Dicharge Port Name`, discharge_country = `Dicharge Port Country`) %>% mutate(market_loc = paste(discharge_port, discharge_country, sep = ", "))

ports = geocode(df_geo$market_loc, source = "google")
  

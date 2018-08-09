# Purpose: Process portfolio data to be joined to shapefile in Talbeau
# Author: Tim Essam, Ph.D. | USAID GeoCenter
# Date: 2018_08_08
# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl")

dir.create("Data")
datapath <- "Data"


# Fix time zone issues
Sys.setenv(TZ = "America/New_York")

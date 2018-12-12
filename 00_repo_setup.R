# Purpose: Process portfolio data to be joined to shapefile in Talbeau
# Author: Tim Essam, Ph.D. | USAID GeoCenter
# Date: 2018_08_08
# Load libraries and data -------------------------------------------------

# INSTALL LIBRARIES - if first time, install pacman
pacman::p_load("tidyverse", "lubridate", "sf", "extrafont", "readxl")

dir.create("Data")
datapath <- "Data"

dir.create("Data_export")
dataexport <- "Data_export"

dir.create("Graphs")
graphpath <- "Graphs"

dir.create("Talbeau")
vizpath <- "Tableau"


# Fix time zone issues
Sys.setenv(TZ = "America/New_York")
# Purpose: Clip Tanzania interpoloated surfaces to 5% or less credible intervals
# Author: Tim Essam, GeoCenter
# Date: 2019_11_21
# Notes: For clipping DHS derived rasters to their 95/90% credible interval levels

pacman::p_load(raster, sp, rgdal, RColorBrewer)

dest_folder <- c("TZ_package_all")
dir(file.path(datapath, dest_folder))


file_list_mean <- as.list(list.files(file.path(datapath, dest_folder), pattern = "MEAN_v01.tif$"))
file_list_ci <- as.list(list.files(file.path(datapath, dest_folder), pattern = "CI_v01.tif$"))

mean_rasters <-  purrr::map(file_list_mean, ~raster(file.path(datapath, "TZ_package_all", .)))
names(mean_rasters) <- file_list_mean %>% set_names()

ci_rasters <- purrr::map(file_list_ci, ~raster(file.path(datapath, "TZ_package_all", .)))
names(file_list_ci) <- file_list_ci %>% set_names()

# Review the credible intervals before running through the model, may need to develop a list of thresholds
# based on the layers. 
purrr::map(ci_rasters, ~median(., na.rm = TRUE))



# Function to execute the mask and return a df ----------------------------


# Function below takes two raster, an input raster that is to be clipped
# a clipper that is the raster upon which the clipped threshold is set
# and it returns a new clipped raster
clipped_raster <- function(input_raster, clipper, threshold = 0.15) {

  threshold_raster <- clipper > threshold
  raster_masked <- mask(input_raster, threshold_raster, maskvalue = TRUE)
  return(raster_masked)
}

# Apply the function across all the rasters, using a threshold that is equal to the median value of the 
# the credible interval surface
tz_clip <- map2(mean_rasters, ci_rasters, ~clipped_raster(.x, .y, median(.y, na.rm = TRUE)))



# Review output -----------------------------------------------------------

my.palette <- brewer.pal(n = 9, name = "Spectral")

# Review the plots to make sure the function is working -- seems to be fine
tz_clip[[11]] %>% plot(col = my.palette)
mean_rasters[[11]] %>% plot(col = my.palette)
plot(ci_rasters[[11]] < .15, col = my.palette)


# Save the raster of interest to the data out folder. Send to colleagues for use in mapping
writeRaster(tz_clip[[11]], file.path(dataout, "TZ2015DHS_FPCUSMWMOD_MS_MEAN_v01_clipped.tif"))



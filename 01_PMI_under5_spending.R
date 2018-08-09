# Purpose: Query DHS API and pull out child mortalilty rates
# Author: Tim Essam, GeoCenter
# Date: 2018_09_08


# install.packages("devtools")
devtools::install_github("murphy-xq/fetchdhs")
library(fetchdhs)

# PUll out all child mortality data for Africa
# List of indicators from DHS:
# https://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html
# What is the crosswalk? 75 is Child mortality tag_id
fetch_tags()%>% 
  filter(str_detect(tag_name,  "Mortality"))


# Convert the list of country codes for Afriac to a vector to pass to DHS function call
  dhs_ids <- country_list %>% 
    filter(Africa_tag == 1) %>% 
    select(DHS_CountryCode) %>% 
    as.list(.) %>% 
    unlist()


  set_return_fields(c("Indicator", "CountryName", "SurveyYear", "SurveyType", "Value"))
  
  # Returns a list of values
  df <- fetch_data(countries = dhs_ids,
                   indicators = c("CM_ECMR_C_CMR", "CM_ECMT_C_CMR"), 
                   years = 2000:2018)
                 






# Purpose: Query DHS API and pull Malaria prevalence according to RDT
# Date: 2018_08_30


# install.packages("devtools")
#devtools::install_github("murphy-xq/fetchdhs")
library(fetchdhs)
library(tidyverse)
library(purrr)
library(llamar)
library(scales)
library(WDI)
source("DHS_country_list.R")

# PUll out all child mortality data for Africa
# List of indicators from DHS:
# https://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html
# What is the crosswalk? 36 is the tag ID
  fetch_tags()%>% 
    filter(str_detect(tag_name,  "Malaria"))
  
  # Better yet, return all indicators with malaria in the definition
  fetch_indicators() %>% 
    filter(str_detect(definition, "malaria")) %>% 
    select(tag_ids, indicator_id, label)

# Convert the list of country codes for Afriac to a vector to pass to DHS function call
  dhs_ids <- country_list %>%
    filter(Africa_tag == 1) %>%
    select(DHS_CountryCode) %>%
    as.list(.) %>%
    unlist()

  set_return_fields(c("Indicator", "CountryName", "SurveyYear", "SurveyType", "Value"))
  #ML_PMAL_C_RDT

# Returns a list of values
dhs_api <- 
  fetch_data(
    countries = dhs_ids,
    indicators = c("ML_PMAL_C_RDT"),
    years = 2006:2018,
    breakdown_level = "subnational"
  )

parasitemia <- 
  map_dfr(dhs_api, ~as.data.frame(.)) 

%>%
  filter(!is.na(indicator)) %>%
  select(-`.`)
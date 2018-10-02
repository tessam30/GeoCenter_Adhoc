# Purpose: Extract DHS Stunting data for last two DHSs
# Author: Tim Essam, Ph.D | USAID GeoCenter
# Date: 2018_09_09
# Audience: Kenya Mission

# Purpose: Query DHS API and pull select indicators for PEPFAR South Africa Training
# Date: 2018_09_07


# install.packages("devtools")
# devtools::install_github("murphy-xq/fetchdhs")
library(fetchdhs)
library(tidyverse)
library(llamar)
library(purrr)
library(sf)
library(styler)


# Better yet, return all indicators with stunting in the definition
fetch_indicators() %>%
  filter(str_detect(definition, "stunt")) %>%
  select(tag_ids, indicator_id, label)

# Check availability by years
# https://api.dhsprogram.com/rest/dhs/countries?returnFields=CountryName,DHS_CountryCode&f=html - country list
data_listing <- fetch_data(countries = c("SN"), tag = 14, years = 2008:2017)
table(data_listing[["df"]][["survey_id"]])


# set_return_fields(c("Indicator", "CountryName", "SurveyYear", "SurveyType", "Value"))
# set_return_fields()
# Returns a list of values
dhs_api <-
  fetch_data(
    countries = c("SN"),
    indicators = c("CN_NUTS_C_HA2"),
    years = 2008:2016,
    breakdown_level = "subnational"
  )

stunting <-
  map_dfr(dhs_api, ~as.data.frame(.)) %>%
  filter(!is.na(data_id))

# Split into two dataframes based on year
df_list <-
  split(stunting, stunting$survey_year)

# List the names in the dataframe list -- these correspond to each year
names(df_list) 

names(df_list) <- c("dhs_2010", "dhs_2012", "dhs_2014", "dhs_2015", "dhs_2016")
list2env(df_list, envir = .GlobalEnv)

# Up to you to figures out -- what does each data frame mean?



# Export data to csvs -----------------------------------------------------

export_list <- list(dhs_2008, dhs_2010, dhs_2012, dhs_2014, dhs_2015, dhs_2016)
names(export_list) <- c("dhs_2008", "dhs_2010", "dhs_2012", "dhs_2014", "dhs_2015", "dhs_201")

export_list %>%
  names() %>%
  map(., ~write_csv(export_list[[.]], paste0(datapath, "/", ., ".csv")))
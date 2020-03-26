# Purpose: List DHS surveys since a given year
# Author: Tim Essam | USAID GH
# Date: 2020_03_26
# Notes: For PG via email


# SETUP -------------------------------------------------------------------

  install.packages("rdhs") #- run 1st time only
  library(rdhs)
  library(tidyverse)

# BUILD API CALL ----------------------------------------------------------

  back_year = 2014

  # list survey characteristics
  sc <- dhs_survey_characteristics()

  ## what are the countryIds - we can find that using this API request
  ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode")) %>%
    pull(DHS_CountryCode)


  ## find all the surveys that match the search criteria
  survs <- dhs_surveys(countryIds = ids, surveyYearStart = back_year) %>%
    select(CountryName, SurveyType, SurveyYearLabel, everything()) %>%
    arrange(CountryName)

  write_csv(survs, "DHS_surveys_back_to_2014.csv")

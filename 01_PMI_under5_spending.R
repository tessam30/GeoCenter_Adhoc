# Purpose: Query DHS API and pull out child mortalilty rates
# Author: Tim Essam, GeoCenter
# Date: 2018_09_08


# install.packages("devtools")
devtools::install_github("murphy-xq/fetchdhs")
library(fetchdhs)
library(purrr)
source("DHS_country_list.R")

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
  #CM_ECMR_C_CMR
  
  # Returns a list of values
  dhs_api <- fetch_data(countries = dhs_ids,
                   indicators = c("CM_ECMT_C_U5M"), 
                   years = 2002:2018, 
                   breakdown_level = "national")

  # map the list to a row-bound data frame using purrr; Can leave url info in or extract dpending on need
  child_morb <- map_dfr(dhs_api, ~ as.data.frame(.)) %>% 
    filter(!is.na(indicator)) %>% 
    select(-`.`)
  
  # Now for the PMI data
  pmi_cm <- read_excel(file.path(datapath, "PMI_MasterSurveyFile_20180123_CC.xlsx"), 
                                 sheet = "Indicator Data") %>% 
    select(Country:`Under-five mortality rate`) %>% 
    filter(Characteristic == "Total") %>% 
    rename(cm = `Under-five mortality rate`) %>% 
    filter(!is.na(cm))

  # What data are coming from the MICS? Need to standardize columns to append data
  # Need the same variables so we can row-bind everything into a single dataframe
  map(list(child_morb, pmi_cm), ~ names(.))
  
  pmi_cm_mics <- 
    pmi_cm %>% 
    filter(grepl("MICS", Survey)) %>% 
    mutate(survey_year = substr(Survey, 1, 4), 
           indicator = "Under-five mortality rate (5 year periods)",
           value = cm, 
           survey_type = "MICS") %>% 
      select(country_name = Country,
             survey_type,
             indicator, 
             survey_year, 
             value)

  cmorbid <- rbind(child_morb, pmi_cm_mics) %>% 
    arrange(country_name, survey_year) %>% 
    rename(year = survey_year, 
           child_mortality = value) %>% 
    mutate(year = as.numeric(year))
  str(cmorbid)

  # Now read in the financial data -- Need better name for file.
  fin_df <- read_tsv(file.path(datapath, "20180806.csv")) %>% 
    
    # Filter out missing budget data -- cannot calculate a total with this as a character too
    filter(BUDGET != "NULL") %>% 
    # Need to escape the $ sign to remove it, also need to coerce to numeric
    mutate(budget = as.numeric(gsub('[^0-9.]', '', BUDGET))) 
  str(fin_df)

  unique(fin_df$COUNTRY)
  
  fin_sum <- 
    fin_df %>% 
    group_by(COUNTRY, FISCAL_YEAR) %>% 
      summarise(total = sum(budget)) %>% 
    mutate(country_name = str_to_title(COUNTRY)) %>% 
    rename(year = FISCAL_YEAR)


  # Next step will be aligning the data for each country by year.
  map(list(cmorbid, fin_sum), ~ names(.))

  pmi_mort <- full_join(cmorbid, fin_sum, by = c("country_name", "year")) %>% 
    arrange(country_name, year)

  
  


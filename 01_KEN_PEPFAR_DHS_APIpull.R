# Purpose: Query DHS API and pull select indicators for PEPFAR South Africa Training
# Date: 2018_09_07


# install.packages("devtools")
#devtools::install_github("murphy-xq/fetchdhs")
library(fetchdhs)
library(tidyverse)
library(purrr)

# Better yet, return all indicators with stunting in the definition
  fetch_indicators() %>% 
    filter(str_detect(definition, "stunt")) %>% 
    select(tag_ids, indicator_id, label)

set_return_fields(c("Indicator", "CountryName", "SurveyYear", "SurveyType", "Value"))
set_return_fields()
# Returns a list of values
  dhs_api <- 
    fetch_data(
      countries = c("KE"),
      indicators = c("CN_NUTS_C_HA2"),
      year = "2014",
      breakdown_level = "subnational"
    )

stunting <- 
  map_dfr(dhs_api, ~as.data.frame(.)) %>% 
  filter(!is.na(data_id))

write_csv(stunting, file.path(dataexport, "KEN_2014_DHS_stunting_subnational.csv"))

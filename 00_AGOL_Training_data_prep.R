# install.packages("devtools")
#devtools::install_github("murphy-xq/fetchdhs")
library(fetchdhs)
library(tidyverse)
library(purrr)

# Better yet, return all indicators with stunting in the definition
fetch_indicators() %>% 
  filter(str_detect(definition, "stunt")) %>% 
  select(tag_ids, indicator_id, label)

set_return_fields(c("Indicator", "CountryName", "SurveyYear", "SurveyType", "Value", ""))
set_return_fields()
# Returns a list of values
dhs_api <- 
  fetch_data(
    countries = c("GY"),
    indicators = c("ML_NETP_H_MOS", "ML_NETP_H_IT2", "CN_NUTS_C_HA2", "CH_FEVT_C_AML"),
    breakdown_level = "subnational"
  )

GY_dhs <- 
  map_dfr(dhs_api, ~as.data.frame(.)) %>% 
  filter(!is.na(data_id)) %>% 
  mutate(REG_ID = region_id)

write_csv(GY_dhs, "GY_2009_DHS_subnational.csv")

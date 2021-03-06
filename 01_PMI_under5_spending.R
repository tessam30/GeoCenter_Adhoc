# Purpose: Query DHS API and pull out child mortalilty rates
# Author: Tim Essam, GeoCenter
# Date: 2018_09_08


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
  dhs_api <- 
    fetch_data(
      countries = dhs_ids,
      indicators = c("CM_ECMT_C_U5M"),
      years = 2002:2018,
      breakdown_level = "national"
  )

  # map the list to a row-bound data frame using purrr; Can leave url info in or extract dpending on need
  child_morb <- 
    map_dfr(dhs_api, ~as.data.frame(.)) %>%
    filter(!is.na(indicator)) %>%
    select(-`.`)
  
  # Now for the PMI data
  pmi_cm <-
    read_excel(file.path(
      datapath,
      "PMI_MasterSurveyFile_20180123_CC.xlsx"),
    sheet = "Indicator Data") %>%
    select(Country:`Under-five mortality rate`) %>%
    filter(Characteristic == "Total") %>%
    rename(cm = `Under-five mortality rate`) %>%
    filter(!is.na(cm))

  # What data are coming from the MICS? Need to standardize columns to append data
  # Need the same variables so we can row-bind everything into a single dataframe
  map(list(child_morb, pmi_cm), ~ names(.))
  
  # pull out the MICS data to add to the DHS stat compiler info
  pmi_cm_mics <-
    pmi_cm %>%
    filter(grepl("MICS", Survey)) %>%
    mutate(
      survey_year = substr(Survey, 1, 4),
      indicator = "Under-five mortality rate (5 year periods)",
      value = cm,
      survey_type = "MICS"
    ) %>%
    select(
      country_name = Country,
      survey_type,
      indicator,
      survey_year,
      value
    )

  # Bind all together to build a foundational data of child mortality
  cmorbid <-
    rbind(child_morb, pmi_cm_mics) %>%
    arrange(country_name, survey_year) %>%
    rename(
      year = survey_year,
      child_mortality = value
    ) %>%
    mutate(year = as.numeric(year)) %>%
    group_by(country_name) %>%
    mutate(data_points = n()) %>%
    ungroup()
  str(cmorbid)
  
  # How many data points per country?
  cmorbid %>%
    filter(data_points != 1) %>%
    mutate(country = fct_reorder(country_name, -child_mortality)) %>%
    ggplot(., aes(x = year, y = child_mortality)) +
    geom_line() +
    geom_point() +
    facet_wrap(~country) +
    theme_ygrid() +
    scale_y_continuous(breaks = c(50, 100, 150, 200))
    


# Load PMI Financial Data -------------------------------------------------

  # Now read in the financial data -- Need better name for file.
  fin_df <- read_tsv(file.path(datapath, "20180806.csv")) %>% 
        # Filter out missing budget data -- cannot calculate a total with this as a character too
    filter(BUDGET != "NULL") %>% 
    # Need to escape the $ sign to remove it, also need to coerce to numeric to math numbers
    mutate(budget = as.numeric(gsub('[^0-9.]', '', BUDGET))) %>% 
    filter(!(COUNTRY %in% c("BURMA", "CAMBODIA", "THAILAND", "GMS REGION")))
    
  str(fin_df)
  unique(fin_df$COUNTRY)

  
  # Need roll ups to get aggregate numbers for final visualization
  fin_sum <-
    fin_df %>%
    # Replace any permutation of Tanzania with a single value for roll-ups
    mutate(COUNTRY = gsub("TANZANIA.*", "TANZANIA", COUNTRY, perl = TRUE)) %>%
    group_by(COUNTRY, FISCAL_YEAR) %>%
    summarise(total = sum(budget)) %>%
    mutate(country_name = str_to_title(COUNTRY)) %>%
    rename(year = FISCAL_YEAR) %>%
    ungroup() %>%
    # fix names and add sortable factors for plotting
    mutate(
      COUNTRY = fct_reorder(COUNTRY, -total),
      total_mil = total / 1e6,
      country_name = ifelse(country_name == "Drc", "Congo Democratic Republic", country_name),
      flag = 1
    )

  
  ggplot(fin_sum, aes(x = year, y = total_mil)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~COUNTRY) +
    theme_xgrid() +
    scale_x_continuous(limits = c(2006, 2016)) +
    scale_y_continuous(label = unit_format(unit = "M"))
    
  # Next step will be aligning the data for each country by year.
  map(list(cmorbid, fin_sum), ~ names(.))

  pmi_mort <- 
    full_join(cmorbid, fin_sum, by = c("country_name", "year")) %>% 
    arrange(country_name, year)


# WDI population indicators -----------------------------------------------
  
  # Need a browserable list of country to get ISO codes for filtering
  world <-
    WDI(
      country = "all",
      indicator = c("SP.POP.TOTL"),
      start = 2017,
      end = 2018
    )

  pmi_countries <- c("AO", "BJ", "CD", "ET", "GH", "GN", "LR", "MG", "MW", "ML", "MZ", 
                     "NG", "RW", "SN", "TZ", "ZM", "ZW")
  
  # Fix country names to be consistent with PMI
  # DRC == Congo, Dem. Rep. == Congo Democratic Republic
  pmi_pop <-
    WDI(
      country = pmi_countries, indicator = c("SP.POP.TOTL"),
      start = 2002, end = 2016
    ) %>%
    mutate(country_name = ifelse(country == "Congo, Dem. Rep.",
      "Congo Democratic Republic",
      country
    )) %>%
    rename(population = SP.POP.TOTL)
  str(pmi_pop)


# Combine datasets and deflate totals by population -----------------------

 pmi_totals_pc <- 
    pmi_mort %>% 
    full_join(., pmi_pop, by = c("country_name", "year")) %>% 
    # Have to sort by total b/c Tableau doesn't like NAs at the top of files (buggy!)
    arrange(country_name, year) %>% 
    mutate(total_pc = total / population, 
           total_pc_fct = fct_reorder(COUNTRY, -total_pc))
    

  #plot results on a scatter
  pmi_totals_pc %>% 
    filter(flag == 1) %>% 
  ggplot(., aes(x = total_pc, y = child_mortality)) +
    geom_point(aes(colour = country_name)) +
    facet_wrap(~year)
  
  # Show PMI per capita spending
  pmi_totals_pc %>% 
    filter(flag == 1) %>% 
  ggplot(., aes(x = year, y = total_pc)) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ total_pc_fct) +
    theme_xgrid() +
    scale_x_continuous(limits = c(2006, 2016)) +
    labs(title = "Liberia has received the most PMI spending on a per capita basis.")
  

# Export to csv for PMI team and Tableau ----------------------------------
  write.csv(pmi_totals_pc, file.path(datapath, "PMIspending_ChildMortality.csv"))
  
  
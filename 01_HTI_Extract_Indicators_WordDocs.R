library(docxtractr)
library(tibble)
library(tidyverse)
library(readxl)


# Read in the data
pirs_files <- list.files(file.path(datapath), pattern = "PIRS")

# Read in all the indicator data and store in a list for accessing tables
pirs_list <- 
  pirs_files %>% 
  map(~ read_docx(file.path(datapath, .)))

# Storing all the tabls in a list to purrr over w/ search terms
pirs_tbls <- 
  pirs_list %>% 
  map(., ~docx_extract_all_tbls(.)) %>% 
  flatten_dfr() 


# Use \\ before paratheses to escape them in the grep call
match_terms <- c("Indicator Name and Number:", 
                 "Definition", "Unit of Measure", 
                 "Disaggregation", "Reporting Level", 
                 "Data Collection Method:", "Data Source\\(s\\):", 
                 "Timing/Frequency of Data Acquisition:", 
                 "Data Analysis:", "Data Review:", 
                 "Known Data Limitations")

# Paste search terms in as vector using Data Pasta
regex = paste(match_terms, collapse = "|")



pirs_df <- 
  pirs_tbls %>% 
  mutate(test = grepl(regex, V1)) %>% 
  filter(test %in% TRUE) %>% 
  select(V1) %>% 
  separate(V1, into = c("terms", "text"), sep = "[:]", extra = "merge" ) %>% 
  mutate(text = str_trim(text, side = "left"),
         flag = ifelse(terms %in% "Indicator Name and Number", 1, 0))

  write_excel_csv(pirs_df, file.path(datapath, "tmp_indicators.csv"))


  






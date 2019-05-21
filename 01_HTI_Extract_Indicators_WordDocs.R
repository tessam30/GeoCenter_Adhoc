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


pirs_df <- 
  pirs_tbls %>% 
  mutate(test = grepl(regex, V1)) %>% 
  filter(test %in% TRUE) %>% 
  select(V1) %>% 
  separate(V1, into = c("terms", "text"), sep = "[:]", extra = "merge" ) %>% 
  mutate(text = str_squish(text))
  
  
  
  write_excel_csv(., file.path(datapath, "tmp_indicators.csv"))


  


# Paste search terms in as vector using Data Pasta

regex = paste(match_terms, collapse="|")


# Now, to iterate through each element of the list and see what words match, flag those entries and extract them into a flat file
pirs_tbls[[1]] %>% mutate(test = grepl(regex, V1)) %>% print(n = Inf) %>% filter(test %in% "TRUE")

pirs_tbls %>% 
  map_df(~ .x %>% mutate_at(vars(V1), list(test = grepl(regex, V1))))

tmp <- map_df(pirs_tbls, ~ mutate(.x, funs(test = grepl(regex, V1))))


         ~mutate(test = grepl(regex, .xV1)))

y %>% map(~ .x %>% mutate_at(vars(-X1), funs(case_when(is.na(.) ~ -9999, 
                                                       TRUE ~ . ))))

library(docxtractr)
library(tibble)
library(tidyverse)


# Read in the data
list.files(file.path(datapath), pattern = "PIRS")
doc <- read_docx(file.path(datapath, "PIRS_TEST.docx"))
doc2 <- read_docx(file.path(datapath, "Reforestation_Projects_PIRS.docx" ))


# indic_raw <- tibble(filename = indicator_files) %>% 
#   mutate(file_contents = map(filename, 
#                              ~read_docx(file.path(datapath, .)))) %>% 
#   mutate(filename = str_remove(filename, ".docx")) %>% 
#   unnest() 


docx_tbl_count(doc2)
docx_describe_tbls(doc2)

tmp <- docx_extract_all_tbls(doc2) 

%>% 
  rename(sheet1 = "USAID.Haiti.Activity.Performance.Indicator.Reference.Sheet")

# Paste search terms in as vector using Data Pasta
# Use \\ before paratheses to escape them in the grep call
match_terms <- c("Indicator Name and Number:", "Definition", "Unit of Measure", "Disaggregation", "Reporting Level", "Data Collection Method:", "Data Source\\(s\\):", "Timing/Frequency of Data Acquisition:", "Data Analysis:", "Data Review:", "Known Data Limitations")

regex = paste(match_terms, collapse="|")


# Now, to iterate through each element of the list and see what words match, flag those entries and extract them into a flat file
tmp <- tmp %>% mutate(test = grepl(regex, sheet1)) %>% print(n = Inf) %>% filter(test %in% "TRUE")

               
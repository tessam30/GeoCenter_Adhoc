library(docxtractr)
library(tibble)
library(dplyr)

# Read in the data
doc <- read_docx(file.path("Data", "PIRS_TEST.docx"))

docx_tbl_count(doc)
docx_describe_tbls(doc)

tmp <- docx_extract_tbl(doc) %>% 
  rename(sheet1 = "USAID.Haiti.Activity.Performance.Indicator.Reference.Sheet")

# Paste search terms in as vector using Data Pasta
# Use \\ before paratheses to escape them in the grep call
match_terms <- c("Indicator Name and Number:", "Definition", "Unit of Measure", "Disaggregation", "Reporting Level", "Data Collection Method:", "Data Source\\(s\\):", "Timing/Frequency of Data Acquisition:", "Data Analysis:", "Data Review:", "Known Data Limitations")

regex = paste(match_terms, collapse="|")

tmp %>% mutate(test = grepl(regex, sheet1)) %>% print(n = Inf) %>% filter(test %in% "TRUE")

               
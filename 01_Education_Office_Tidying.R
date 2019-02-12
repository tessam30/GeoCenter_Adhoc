##%######################################################%##
#                                                          #
####                  Education Data                    ####
#                                                          #
##%######################################################%##
# Purpose: Explore 

library(tidyverse)
library(readxl)

excel_sheets(file.path(datapath, "Education Sector Learning Inventory SpreadsheetV3- Revision Draft.xlsx"))

df <- read_excel(file.path(datapath, "Education Sector Learning Inventory SpreadsheetV3- Revision Draft.xlsx"), sheet = "Responses Overview") %>% 
  filter(!is.na(`Research Product (Product Name)`))



df %>% group_by(`Research Product (Product Name)`) %>% count()  %>% print(n = Inf)               

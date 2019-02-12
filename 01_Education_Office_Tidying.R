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

# Creating a function to quickly summarize the different occurences within a column
tab_that <- function(...) {
  grouping <- enquos(...)
  df %>% 
    group_by(!!!grouping) %>% 
    count() %>% 
    arrange(desc(n)) %>% 
    print(n = Inf) #%>% 
    #knitr::kable() # to print to a markdown format
} 

# Look at the universe of options to choose from
df %>% names()
# Iteratively see how many common occurences there are
tab_that(`Research Topic (key words)`)


# Loop over columns and summarise stuff
df %>% 
  select(`Education Strategy Area(s)`:`Research Approach`) %>% 
  map(tab_that)

# For this first column, I'd parse each entry based on commas, then restack it.
# This becomes
tmp <- df %>% 
  
  # Keeping just two columns to show workflow
  select(`Research Product (Product Name)`, `Education Strategy Area(s)`) %>% 
  
  # split out the answers using a delimiter -- commas may not work always due to Google forms
  separate(`Education Strategy Area(s)`, into = c("a", "b", "c", "d"), sep = ",") %>% 
  
  # gather data back together and then drop any rows with missing values for Strategy_areas
  gather(key = col_index, value = "Strategy_areas", -`Research Product (Product Name)`) %>% 
  na.omit(Strategy_ares)

# This would be the general workflow. The difficult part is determining which columns he would need to do this operation for and how to keep the information together w/out growing the table too much
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
tab_that(`Research Activity status`)


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

# So what would this look like if were to carry out this process for a single research project?
#dput(names(sample_df))

sample_df <- 
  df %>% 
  mutate(id = row_number()) %>% 
  select(id, everything()) %>% 
  #filter(id %in% c(27)) %>% # Rename everything as names are too long
  rename(product = `Research Product (Product Name)`,
         strategy = `Education Strategy Area(s)`, 
         topic = `Research Topic (key words)`, 
         approach = `Research Approach`, 
         OU = `Managing OU`, 
         POC = `USAID POC`, 
         location = `Reference Location: country, region, and/or global`, 
         country = `Specific country(ies) of the learning activity`, 
         ref_pop = `Reference Population`, 
         status = `Research Activity status`, 
         start_date = `Research Activity Start Date`, 
         end_date = `Learning \"Deliverable\" date: when is a report on learning coming out?`, 
         purpose =`Purpose: To what end are you doing this learning?`, 
         audience = `Who is the learning for?`, 
         mechanism = `Mechanism name\n(if applicable)`, 
         q1 = `Question 1`, 
         q2 = `Question 2 (If applicable)`, 
         q3 = `Question 3 (If applicable)`, 
         q4 = `Question 4 (If applicable)`, 
         q5 = `Question 5 (If applicable)`, 
         q6 = `Question 6 (If applicable)`, 
         q7 = `Question 7 (If applicable)`, 
         q8 = `Question 8 (If applicable)`, 
         other = `Other Additional Questions (If applicable)`)

# First, let's spread out the country into proper columns and then reshape it
library(splitstackshape)

sample_df_long <- 
  sample_df %>% 
  gather("question_num", "text", q1:q8) %>% 
  filter(!is.na(text)) %>% 
  cSplit("country", sep = ',', direction = "wide", drop = FALSE) 

  
# What if you only focus on geography
  tmp <- sample_df %>% 
  select(id, product, country) %>% 
  cSplit("country", sep = ',', direction = "wide", drop = FALSE) %>% 
    gather("geography", "place", country_01:country_20) %>% 
    filter(!is.na(place))
    
    

# Purpose: Reshape Power Africa Data
# Authoer: TE / JMKC
# Date: 2018/08/08

# Pre-processing notes
# 1) Determine whether or not you can just recalculate the percentiles in Excel or in R
# 2) Somebody thought it was cool to literally write in "NA" for missing values in the spreadsheet 
# versus leaving it blank. Leaving blank best. NA write-ins, not cool.



# Load libraries and determine data path ---------------------------------------
# What libraries are required to read in the data?
library(tidyverse)
library(readxl)
library(magrittr)


# Where does your data live? You'll have to change this on your machine
(datapath) 
data <- c("West Africa_Enabling Environment Scorecard Mockup.xlsx")


# Load the data
  df <- read_excel(file.path(datapath, data), sheet = "Calculate Percentile") %>% 
    select(-starts_with("Percentile")) %>% 
    rename(DB = `DB 2018`)

# Check how the data are read in -- do some come in as non-numerical? Not any more.
  str(df)  

# Let's get this stuff in shape. First, some gathering.
  df_shaped <- 
    df %>% 
    gather(., key = "metric", value = "value", DB:`PPP Procurement`) %>% 
    group_by(metric) %>% 
    # Recalculate the grouped percentile rank
    mutate(percent_rank = percent_rank(value))


# Export and expand -------------------------------------------------------
  write_csv(df_shaped, file.path(datapath, "WAEES_data_2018_0808.csv"))


# One plot for good measure
  df_shaped %>% 
    mutate(country = as.factor(Economy), 
           country = fct_reorder(country, -percent_rank)) %>% 
    ggplot(., aes(x = percent_rank, y = country)) + 
    geom_point() + 
    facet_wrap(~ metric) 

# One quick PCA to see if it even makes sense -- too much missing data
  df_pca <-  
    df_shaped %>% 
    select(-percent_rank) %>% 
    spread(., metric, value) %>% 
    
    # set row names, Keep cols for PCA
    column_to_rownames('Economy') %>% 
    select(DB:`Utility Creditworthiness`) 
  
  # For later, when you go down the PCA path
  pca_out <- prcomp(na.omit((df_pca)), scale = TRUE)
  biplot((pca_out), var.axes = TRUE, scale = 0, cex = c(1, 1))




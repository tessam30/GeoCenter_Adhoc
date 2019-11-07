``` r
library(tidyverse)
library(haven)
library(sjlabelled)
library(memisc)
library(Hmisc)
library(datapasta)

# Using a simple cross-walk cobbled together from a pasted tribble
# you can create a simple cross-walk in Excel and then use the datapasta add in to R-studio
# to paste the CW as a tribble or dataframe
landprep_cw <- 
  tibble::tribble(
    ~V7101, ~V7107_num,                                 ~V7107_label,
       "A",          1,                       "Did not prepare land",
       "B",          2,                  "Zero tillage methods only",
      "BC",          3,         "Zero tillage methods and ploughing",
      "BX",          4, "Zero tillage methods and some other method",
       "C",          5,                             "Ploughing only",
      "CX",          6,            "Ploughing and some other method",
       "X",          7,                          "Some other method",
        NA,          9,                         "Not a maize farmer"
    ) 

# Sample of the data you provided
df <- tibble::tribble(
  ~V7101, ~freq,
     "A",     1,
     "B",     2,
    "BC",     3,
    "BX",     4,
     "C",     5,
    "CX",     6,
     "X",     7,
      NA,     8,
  ) 
  
# This chunk below matches combines and ifelse statement with the %in% (inlist equivalent from Stata) operator
# to match columns in the dataframe w/ columns in the crosswalk and then return a
df_cw <- df %>% 
  mutate(landprep_m = ifelse(V7101 %in% landprep_cw$V7101, landprep_cw$V7107_num, 9))

# OR you can just merge the crosswalk into the origninal data
df_merge <- df %>% 
  left_join(., landprep_cw, by = c("V7101" = "V7101"))


# Using the sjlabelled package + case_when function -----------------------
# Create a new variable called land prep that takes values 1 - 9 using case_when function
# Use the sjlabelled package to apply the labels to the new variable

df2 <- 
  df %>% 
  mutate(landprep_m = case_when(
    V7101 == 'A' ~ 1,
    V7101 == 'B' ~ 2,
    V7101 == 'BC' ~3,
    V7101 == 'BX' ~4,
    V7101 == 'C'  ~5,
    V7101 == 'CX' ~6,
    V7101 == 'X'  ~7,
    TRUE ~ 9)
    ) %>% 
  set_labels(landprep_m, labels = c("Did not prepare land" = 1,
                                     "Zero tillage methods only" = 2,
                                    "Zero tillage methods and ploughing" = 3,
                                    "Zero tillage methods and some other method" = 4,
                                    "Ploughing only" = 5,
                                    "Ploughing and some other method" = 6,
                                    "Some other method" = 7,
                                    "Not a maize farmer" = 9))

str(df2)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    8 obs. of  3 variables:
#>  $ V7101     : chr  "A" "B" "BC" "BX" ...
#>  $ freq      : num  1 2 3 4 5 6 7 8
#>  $ landprep_m: num  1 2 3 4 5 6 7 9
#>   ..- attr(*, "labels")= Named num  1 2 3 4 5 6 7 9
#>   .. ..- attr(*, "names")= chr  "Did not prepare land" "Zero tillage methods only" "Zero tillage methods and ploughing" "Zero tillage methods and some other method" ...
attr(df2$landprep_m, "label") # Check the labels to see if they are correct
#>                       Did not prepare land 
#>                                          1 
#>                  Zero tillage methods only 
#>                                          2 
#>         Zero tillage methods and ploughing 
#>                                          3 
#> Zero tillage methods and some other method 
#>                                          4 
#>                             Ploughing only 
#>                                          5 
#>            Ploughing and some other method 
#>                                          6 
#>                          Some other method 
#>                                          7 
#>                         Not a maize farmer 
#>                                          9


# Using factors method ----------------------------------------------------------
       
df3 <- 
  df %>% 
  mutate(landprep_m = case_when(
    V7101 == 'A' ~ 1,
    V7101 == 'B' ~ 2,
    V7101 == 'BC' ~3,
    V7101 == 'BX' ~4,
    V7101 == 'C'  ~5,
    V7101 == 'CX' ~6,
    V7101 == 'X'  ~7,
    TRUE ~ 9 
  )) %>% 
  mutate(landprep_fct = factor(landprep_m, 
                               labels = c("Did not prepare land",
                                          "Zero tillage methods only",
                                          "Zero tillage methods and ploughing",
                                          "Zero tillage methods and some other method",
                                          "Ploughing only",
                                          "Ploughing and some other method",
                                          "Some other method",
                                          "Not a maize farmer")))
df3 %>% str()
#> Classes 'tbl_df', 'tbl' and 'data.frame':    8 obs. of  4 variables:
#>  $ V7101       : chr  "A" "B" "BC" "BX" ...
#>  $ freq        : num  1 2 3 4 5 6 7 8
#>  $ landprep_m  : num  1 2 3 4 5 6 7 9
#>  $ landprep_fct: Factor w/ 8 levels "Did not prepare land",..: 1 2 3 4 5 6 7 8
labels(df3$landprep_fct)
#> [1] "1" "2" "3" "4" "5" "6" "7" "8"

# A final option is to just recode the V7101 values directly to strings using a case when statement
df3 <- 
  df %>% 
  mutate(landprep_m = case_when(
    V7101 == 'A' ~ "Did not prepare land",
    V7101 == 'B' ~ "Zero tillage methods only",
    V7101 == 'BC' ~ "Zero tillage methods and ploughing",
    V7101 == 'BX' ~  "Zero tillage methods and some other method",
    V7101 == 'C'  ~ "Ploughing only",
    V7101 == 'CX' ~ "Ploughing and some other method",
    V7101 == 'X'  ~ "Some other method",
    TRUE ~ "Not a maize farmer" # this is saying for all other cases not listed above, give them a value of 9
  )) %>% 
  mutate(landprep_fct = factor(landprep_m, as.ordered(landprep_m)))

df3 %>% str()
#> Classes 'tbl_df', 'tbl' and 'data.frame':    8 obs. of  4 variables:
#>  $ V7101       : chr  "A" "B" "BC" "BX" ...
#>  $ freq        : num  1 2 3 4 5 6 7 8
#>  $ landprep_m  : chr  "Did not prepare land" "Zero tillage methods only" "Zero tillage methods and ploughing" "Zero tillage methods and some other method" ...
#>  $ landprep_fct: Factor w/ 8 levels "Did not prepare land",..: 1 2 3 4 5 6 7 8
levels(df3$landprep_fct) # notice the levels do not align exactly to the values associated w/ landprep type
#> [1] "Did not prepare land"                      
#> [2] "Zero tillage methods only"                 
#> [3] "Zero tillage methods and ploughing"        
#> [4] "Zero tillage methods and some other method"
#> [5] "Ploughing only"                            
#> [6] "Ploughing and some other method"           
#> [7] "Some other method"                         
#> [8] "Not a maize farmer"
```

<sup>Created on 2019-11-07 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0.9000)</sup>

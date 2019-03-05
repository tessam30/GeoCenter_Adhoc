## %######################################################%##
#                                                          #
####                  Education Data                    ####
#                                                          #
## %######################################################%##
# Purpose: Explore

library(tidyverse)
library(readxl)
library(data.table)


excel_sheets(file.path(datapath, "Education Sector Learning Inventory SpreadsheetV3- Revision Draft.xlsx"))

df <- read_excel(file.path(datapath, "Education Sector Learning Inventory SpreadsheetV3- Revision Draft.xlsx"), sheet = "Responses Overview") %>%
  filter(!is.na(`Research Product (Product Name)`))

# Use lat/lon based make-up of the world; More simplified than other one, but country names are different
world2 <- map_data("world")

# Creating a function to quickly summarize the different occurences within a column
tab_that <- function(...) {
  grouping <- enquos(...)
  df %>%
    group_by(!!!grouping) %>%
    count() %>%
    arrange(desc(n)) %>%
    print(n = Inf) # %>%
  # knitr::kable() # to print to a markdown format
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
# dput(names(sample_df))

sample_df <-
  df %>%
  mutate(id = row_number()) %>%
  select(id, everything()) %>%
  # filter(id %in% c(27)) %>% # Rename everything as names are too long
  rename(
    product = `Research Product (Product Name)`,
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
    purpose = `Purpose: To what end are you doing this learning?`,
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
    other = `Other Additional Questions (If applicable)`
  ) %>%
  mutate(
    country = ifelse(id == 35, "Cambodia,Nepal,Malawi", country),
    country = ifelse(id == 75, "Asia,Africa, Middle East", country),
    country = ifelse(id %in% c(65, 66), "Kyrgyzstan, Tajikistan", country),
    country = ifelse(id == 5, "Peru", country)
  )

# First, let's spread out the country into proper columns and then reshape it
library(splitstackshape)
library(ggmap)
library(mapdata)
library(maptools)

sample_df_long <-
  sample_df %>%
  gather("question_num", "text", q1:q8) %>%
  filter(!is.na(text)) %>%
  cSplit("country", sep = ",", direction = "wide", drop = FALSE)


# What if you only focus on geography
educ <-
  sample_df %>%
  select(id, product, country) %>%
  cSplit("country", sep = ",", direction = "wide", drop = FALSE) %>%
  gather("geography", "place", country_01:country_20) %>%
  filter(!is.na(place)) %>%
  mutate(
    place = case_when(
      place %like% c("Asia|Asia region|Asia Region") ~ "Asia",
      place %like% c("and Nigeria") ~ "Nigeria",
      place %like% c("WB/Gaza|West Bank|West Bank and Gaza|West Bank Gaza") ~ "West Bank Gaza",
      str_detect(place, c("global|Global")) ~ "Global",
      str_detect(place, "Afghanistan") ~ "Afghanistan",
      str_detect(place, "El Salvador") ~ "El Salvador",
      str_detect(place, "DRC|Democratic Republic of Congo") ~ "Democratic Republic of the Congo",
      str_detect(place, "Peru") ~ "Peru",
      str_detect(place, "South Sudan") ~ "South Sudan",
      str_detect(place, "Kyrgyz") ~ "Kyrgyzstan",
      str_detect(place, c("N/A|TBD")) ~ "Not available",
      str_detect(place, "Syria") ~ "Syrian Arab Republic",
      str_detect(place, "Vietnam") ~ "Viet Nam",
      str_detect(place, "Laos") ~ "Lao People's Democratic Republic",
      str_detect(place, "Tanzania") ~ "United Republic of Tanzania",
      TRUE ~ as.character(place)
    ),
    place_flag = case_when(
      place %like% c("Sub-Saharan Africa|Not Available|Middle East|LAC Region|Central America|Caribbean|Asia|Not available|Africa|Global|LAC region") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  arrange(id)

educ %>%
  group_by(place, place_flag) %>%
  count() %>%
  arrange(desc(place_flag)) %>%
  print(n = Inf)

# Attempt to map out where the remaining projects are
# Non-joiners
# Ivory Coast - Cote d'Ivoire
# South Sudan
#  - Tajikistan
#  - Kyrgyzstan
#  Myanmar - Burma
#  West Bank? - West Bank Gaza
# Democratic Republic of the Congo

data(wrld_simpl)
world <- st_as_sf(wrld_simpl) # set world polygon boundaries as a join field

# Determine if you need to change country names in the using dataset
intersect(educ$place, wrld_simpl$NAME)
setdiff(educ$place, wrld_simpl$NAME) # What countries do not join up

map <-
  educ %>%
  mutate(educ_flag = 1) %>%
  right_join(., world, by = c("place" = "NAME")) %>%
  group_by(place) %>%
  mutate(count = sum(educ_flag, na.rm = TRUE)) %>%
  ungroup()


# Map of reserach products by country count -------------------------------

map %>%
  ggplot() +
  geom_sf(aes(group = ISO3), fill = "#bababa", colour = "#FFFFFF", size = 0.1) +
  geom_sf(
    data = map %>% filter(educ_flag == 1),
    aes(fill = count), colour = "white", size = 0.5
  ) +
  scale_fill_viridis_c(option = "A", direction = -1, end = 0.9) +
  coord_sf(xlim = c(-100, 150), ylim = c(-50, 50), expand = FALSE) +
  theme(legend.position = "top") +
  labs(
    title = "Nepal, Mali and West Bank Gaza have the most research products.",
    subtitle = "Global and regional research efforts omitted.",
    fill = "total products",
    caption = "Source: USAID Education Learning Inventory Database"
  )

ggsave(file.path(graphpath, "EDUC_geography_summary.pdf"),
  plot = last_plot(),
  height = 8.5, width = 11, units = "in"
)


# Plot research products by geographic region -----------------------------

# Set labels for overriding the 0 and 1s
place_codes <- c(
  "0" = "Country information available",
  "1" = "Country information ambiguous"
)


educ %>%
  group_by(place, place_flag) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  mutate(
    sort_place = fct_reorder(place, n),
    place_flag = as.character(place_flag)
  ) %>%
  ggplot(aes(sort_place, n)) +
  geom_col(fill = "#bababa") +
  coord_flip() +
  theme_minimal() +
  theme(strip.text = element_text(hjust = 0)) +
  facet_wrap(~place_flag,
    scales = "free",
    labeller = as_labeller(place_codes)
  ) +
  labs(
    x = "",
    y = "Number of research projects",
    title = "The geographic focus of nine research products is unknown.",
    subtitle = "West Bank Gaza, Nepal and Mali have the most research products.",
    caption = "Source: USAID Education Learning Inventory Database"
  )

ggsave(file.path(graphpath, "EDUC_geography_bargraph.pdf"),
  plot = last_plot(),
  height = 8.5, width = 11, units = "in"
)


# Export data for sharing w/ education folks ------------------------------

write_csv(educ, file.path(datapath, "EDUC_geography_example.csv"))

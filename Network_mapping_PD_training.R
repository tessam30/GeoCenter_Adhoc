library(tidyverse)
library(igraph)

# https://www.jessesadler.com/post/network-analysis-with-r/


node <- tibble::tribble(
              ~from,            ~Type,
    "Coffee Farmers",     "Key Player",
   "Input Suppliers",  "Keep Informed",
  "Credit Suppliers", "Meets the Need",
              "GPCU",     "Key Player",
         "Exporters",        "Monitor",
        "GRET AMEXA",     "Key Player",
    "GRET Min of Ag",     "Key Player",
    "GRET President", "Meets the Need"
  ) %>% 
  mutate(from_id = row_number())

edge <- 
  tibble::tribble(
                ~from,                 ~to, ~strength,
      "Coffee Farmers",  "Input Suppliers",    "weak",
     "Input Suppliers", "Credit Suppliers",    "weak",
                "GPCU",        "Exporters",  "medium",
           "Exporters",       "GRET AMEXA",  "strong",
          "GRET AMEXA",   "GRET Min of Ag",  "strong",
      "GRET Min of Ag",   "GRET President",    "weak",
     "Input Suppliers",             "GPCU",  "medium",
    "Credit Suppliers",        "Exporters",    "weak",
                "GPCU",       "GRET AMEXA",   "strong",
           "Exporters",   "GRET Min of Ag",  "medium",
          "GRET AMEXA",   "GRET President",    "weak",
      "GRET Min of Ag",   "Coffee Farmers",    "weak",
      "Coffee Farmers",             "GPCU",    "weak",
    "Credit Suppliers",       "GRET AMEXA",  "medium",
                "GPCU",   "GRET Min of Ag",  "medium",
      "GRET Min of Ag",  "Input Suppliers",  "medium",
     "Input Suppliers",       "GRET AMEXA",    "weak"
    ) %>% left_join(., node, by = c("to" = "from")) %>% 
  rename(to_id = from_id) %>% 
  select(-Type)

df <- 
  node %>% 
  left_join(., edge, by = c("from")) %>% 
  mutate(value = case_when(
         strength == "weak" ~ 1,
         strength == "medium" ~ 2,
         strength == "strong" ~3, 
         TRUE ~ NA_real_)
  )

# Use the igraph package to generate a graph of the connections

routes_igraph <- graph_from_data_frame(d = edge, vertices = node, directed = TRUE)
routes_igraph

plot(routes_igraph, edge.arrow.size = 0.2)
plot(routes_igraph, layout = layout_with_graphopt, edge.arrow.size = 0.2, )




library(visNetwork)
library(networkD3)

# Stricter requirements on the column names
nodes <- 
  node %>% 
  rename(id = from_id)

edges <- 
  df %>% 
  rename(from = from_id,
         to = to_id)

visNetwork(nodes, edges)

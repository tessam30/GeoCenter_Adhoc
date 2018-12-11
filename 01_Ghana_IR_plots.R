# Import Excel data for PMI, reshape and plot as bars
# Date: 2018_10_01
# Author: Tim Essam
# Audience: GeoCenter


# Load data ---------------------------------------------------------------

# What is loadedin the session?
library(ggthemes)
library(llamar)
library(scales)
sessionInfo()

df <- read_excel(file.path(datapath, "GHANA_IR_Updated.xlsx"), sheet = "GH_ITN_Resistance")
colors <- read_excel(file.path(datapath, "GHANA_IR_Updated.xlsx"), sheet = "Colors_r")

# Convert the color palette to hex and use crosswalk to map colors
colors <- 
  colors %>%  
  mutate(hex = rgb(red = Red, green = Green, blue = Blue, maxColorValue = 255))
 
df_long <- 
  df %>% 
  gather(., MinMort:PE_PBOControl, key = "drug", value = "effectiveness") %>% 
  left_join(., colors, by = c("drug" = "Drug"))%>% 
  mutate(hex = ifelse(drug == "MinMort", "#d2d2d2", hex),
         label = percent(round(effectiveness, 2)),
         effness_zeros = ifelse(is.na(effectiveness), 0, effectiveness))  

# Set the colors for each category -- done using scale_fill_identity()
# Need to build a function to rapidly iterate through all the levels of ADM2, 
# plot the drugs, drop missing values, and save as a plot

# First, let's test the colors
df_long %>% 
  filter(!is.na(effectiveness)) %>% 
ggplot(., aes(x = ADM2, y = effectiveness, fill = hex)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ drug) +
  scale_fill_identity()


adm2_plot <- function(df, admin2) {
  df %>% 
    filter(ADM2 == admin2, drug != "MinMort") %>% 
    filter(!is.na(effectiveness)) %>%
    ggplot(., aes(x = drug, 
                  y = effectiveness,
                  fill = hex)) +
    geom_col() +
    coord_flip() +
    scale_fill_identity() +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
    ggtitle(str_c("Drug effectiveness for ", admin2)) +
    labs(x = "", y = "drug effectiveness") +
    theme_tufte()
}

adm2_plot(df_long, "Asante Akim North District")



# Using purrr with nesting and ggplot embedded within a mutate. Adopted from below
# https://rstudio-pubs-static.s3.amazonaws.com/294422_099df24a1c0f46f99db848ca5c48ff6b.html

plots <- df_long %>%
  filter(!is.na(effectiveness)) %>%  
  filter(drug != "MinMort") %>% 
  group_by(ADM2) %>% 
  nest() %>% 
  mutate(plot = map(data, ~ ggplot(., aes(x = drug, y = effectiveness, fill = hex)) + 
                      geom_col() +
                      scale_fill_identity() + 
                      scale_y_continuous(limits = c(0, 1), labels = scales::percent)+
                      coord_flip() +
                      labs(x = "", y = "") +
                      geom_text(aes(label = label),
                                colour = grey50K, 
                                position=position_dodge(width=0.2),
                                hjust = -0.25, 
                                family="Lato Light") +
                      theme(axis.ticks = element_blank()) +
                      geom_hline(aes(yintercept = 0.85), 
                                 colour = grey50K, 
                                 linetype ="dotted") +
                      theme_yaxis()), 
         filename = str_c(ADM2, ".png")) %>% 
  select(filename, plot)

pwalk(plots, ggsave, path = file.path(datapath))

# Make similar plots except change the pivot point to be the drug effectiveness sorted by district
drug_plots <- df_long %>% 
  # filter(!is.na(effectiveness)) %>%  
  filter(drug != "MinMort") %>% 
  mutate(ADM2_sort = factor(ADM2), 
         drug_name = drug) %>% 
  group_by(drug) %>% 
  nest() %>% 
  mutate(plot = map(data, ~ggplot(., aes(x = fct_reorder(ADM2_sort, effness_zeros, desc = "TRUE"), 
                             y = effectiveness, fill = hex)) +
           geom_col(position = position_dodge(preserve = "single")) +
           scale_fill_identity() + 
           scale_y_continuous(limits = c(0, 1), labels = scales::percent)+
           coord_flip() +
           labs(x = "", y = "", title = .$drug_name) +
            geom_text(aes(label = label),
                      colour = grey50K, 
                      position=position_dodge(width=0.2),
                      hjust = -0.25, 
                      family="Lato Light") +
             geom_hline(aes(yintercept = 0.85), 
                        colour = grey50K, 
                        linetype ="dotted") +
             annotate("text", y = 0.72, x = 1, label = "85% target rate", 
                      family ="Lato Light", colour = grey50K) +
             theme_yaxis()),
  filename = str_c(drug, ".png")) %>% 
  select(filename, plot)

pwalk(drug_plots, ggsave, path = file.path(datapath)) 
         
write_csv(df_long, file.path(datapath, "Ghana_IR_plots.csv"))



  




  
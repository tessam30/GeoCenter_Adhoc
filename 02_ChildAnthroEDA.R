# Plot and explore statistics of Childrens' anthropometric data for DHS
# Date: 2017_12_05
# Author: Tim Essam, GeoCenter
# Dependencies: 00, and 01, dhs_helpers


# Data notes --------------------------------------------------------------

# Child malnutrition information for wasting, underweight and body mass index
# are missing in the dataset. 





# Check for data and functions --------------------------------------------

if (exists('kids') && is.data.frame(get('kids'))) {
  print("Childrens anthropometric data is ready")
  } else {
    source(str_c(base_dir, '01_ChildAnthro.R'))
  }



# Plot variables and annotations ------------------------------------------

# Geoenter information with padding on the right
caption  <- caption_graph('GeoCenter December 2017', 'Source: 2008-09 Demographic and Health Survey', padding = 30)

theme_caption <- function(size = 10, colour = grey30K) {  

  # hjust = 0.5 moves caption to the middle of the plot
  theme(plot.caption = element_text(size = size, colour = colour, hjust = .95))
}

# for annotations see: https://rud.is/b/2016/03/16/supreme-annotations/


# Basic plots to reproduce in Tableau -------------------------------------

# P1: Create a tibble of official DHS data overtime to show time series plot

stunting_time <- tibble::tribble(
  ~Country,        ~Survey, ~Year, ~stunting,
  "Madagascar",  "2008-09 DHS", 2008,      .501,
  "Madagascar",  "2003-04 DHS", 2003,      .532,
  "Madagascar",     "1997 DHS", 1997,        .55,
  "Madagascar",     "1992 DHS", 1992,      .604,
  "Mozambique", "2011 DHS", 2011, .426,
  "Mozambique", "2003 DHS", 2003, .470,
  "Mozambique", "1997 DHS", 1997, .424
)

p1 <-  ggplot(stunting_time %>% filter(Country == "Madagascar"), 
       aes(x = Year, y = stunting, fill = stunting, group = Country)) + 
  geom_line(colour = grey20K) +
  geom_point(size = 6, shape = 21, colour = grey90K) +
  scale_fill_gradientn(colours = llamar::RdPu[4:9]) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1))  +
  theme_xygrid()
p1

write_csv(stunting_time, str_c(base_dir, out_dir, 'MDG_stunting_overtime.csv'))
          
ggplot(kids, aes(x = as.factor(wealth_cat), y = stunting)) + 
  geom_violin()


# P2: bar graph example: stunting and wasting by region ------------------------------------------
stunting_region = kids %>% calcPtEst(var = 'stunted', by_var = 'region_lab',
                               use_weights = TRUE, weight = 'svywt',
                               strata = 'strata', psu = 'psu')

p2 <- ggplot(stunting_region, aes(x = fct_reorder(region_lab, stunted), y = stunted)) +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = 'identity', fill = grey40K) + 
  coord_flip() +
  labs(title = "Madagascar stunting by region",
       subtitle = "Regions sorted in descending order",
       caption = caption) +
  theme_ygrid() +
  theme_caption() +
  theme(axis.title.y = element_blank())
p2

write.csv(stunting_region, str_c(base_dir, out_dir, 'MDG_stunting_region.csv'))


# P3: Plot stunting by WI -----------------------------------------------------
stunting_wealth= calcPtEst(kids, var = 'stunted', 
                          by_var = 'wealth_cat', use_weights = TRUE, 
                          psu_var = 'psu', strata_var = 'strata', 
                          weight_var = 'svywt') %>%
  labelDataset(.)


p3 <- ggplot(stunting_wealth, aes(x = avg, y = wealth_cat, fill = avg)) + 
  geom_segment(aes(x = lb, xend = ub, y = wealth_cat, yend = wealth_cat), colour = grey15K, size = 2) +
  geom_point(size = 4, shape = 21, colour = grey90K) +
  scale_y_continuous(labels = c('lowest', 'low', 'middle', 'high', 'highest')) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + 
  scale_fill_gradientn(colours = llamar::"Greens") +
  coord_flip() +
  labs(title = 'Stunting rates by wealth quintile', caption = caption) +
  theme_caption() +
  theme_ygrid()
p3

write_csv(stunting_wealth, str_c(base_dir, out_dir, "MDG_stunting_wealth.csv"))


# P4: Stunting rates by age categories ----------------------------------------

# Merge in results from stat call in order to create correct facet order based on stunting rates
p4 <- kids %>% left_join(stunting_region, by = "region_lab") %>% 
  
  # Use the stunting values from the grouped point estimates as the ordering
  mutate(region_lab = fct_reorder(region_lab, -stunted.y)) %>%
  filter(!is.na(age_months)) %>% 
  group_by(age_months) %>% 
  
  # Create reference loess plot data
  mutate(stunt_mo_ave = mean(stunted.x, na.rm = TRUE)) %>% 
  ungroup() %>% 
  
  # Start plot code
  ggplot(., aes(x = age_months, y = stunted.x)) +
  
  # Set the reference line to be behind the main plot
  geom_hline(aes(yintercept = stunted.y, colour = stunted.y, alpha = 0.3), linetype = "dashed") + 
  scale_colour_distiller(direction = 1, palette = "PuRd") +
  geom_smooth(colour = "#de77ae", fill = grey10K) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = seq(0, 60, by = 12)) +
  ggtitle('Stunting rates generally increase with age across regions', 
          subtitle = "Dotted line reflects region average (Source: DHS 2009)") +
  
  # Use labeller option to capitalize regions in the facets
  facet_wrap(~region_lab, labeller = labeller(region_lab = capitalize)) +
  theme(text = element_text("Lato"))+
  theme_xygrid() +
  theme_caption() +
  theme(panel.spacing = unit(1, "lines"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
p4

# P5: Z-scores across wealth index --------------------------------------------
p5 <- kids %>% left_join(stunting_region, by = "region_lab") %>% 
  
  # Use the stunting values from the grouped point estimates as the ordering
  mutate(region_lab = fct_reorder(region_lab, -stunted.y)) %>%
  filter(!is.na(stunting)) %>% 
  
  ggplot(., aes(x = stunting, y = wealth)) +
  
  geom_point(alpha = 0.05) +
  geom_smooth(colour = "#de77ae", fill = grey30K) +
  facet_wrap(~region_lab, labeller = labeller(region_lab = capitalize)) +
  ggtitle('Stunting z-scores do not appear to be correlated with asset holdings',
          subtitle = '') +
  labs(caption = caption, x = "stunting z-score", y = "wealth index") +
  theme_caption() +
  theme_xygrid() +
  theme(panel.spacing = unit(1, "lines"))
p5


# P6: Stunting by education category ------------------------------------------

stunting_educ <- kids %>% calcPtEst(var = 'stunted', by_var = 'educ_high_lab',
                  use_weights = TRUE, weight = 'svywt',
                  strata = 'strata', psu = 'psu') %>% 
  labelDataset()

p6 <- ggplot(stunting_educ, aes(x = avg, y = educ_high_lab, fill = avg)) + 
  geom_segment(aes(x = lb, xend = ub, y = educ_high_lab, yend = educ_high_lab), colour = grey15K, size = 2) +
  geom_point(size = 4, shape = 21, colour = grey90K) +
  #scale_y_continuous(labels = c('lowest', 'low', 'middle', 'high', 'highest')) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + 
  scale_fill_gradientn(colours = llamar::RdPu) +
  coord_flip()  +
  ggtitle('Stunting rates fall with educational attainment', 
          subtitle = 'But the only with post-secondary') +
  labs(caption = caption) +
  theme_caption() +
  theme_ygrid() +
  theme(axis.title.y = element_blank())
p6

write_csv(stunting_educ, str_c(base_dir, out_dir, "MDG_stunting_education.csv"))

#P7:  Summary statistics with weights -----------------------------------------
calcPtEst(kids, var = 'stunted', use_weights = TRUE, 
          psu_var = 'psu', strata_var = 'strata', 
          weight_var = 'svywt')

# Estimate stunting by age category and create a new dataframe with results
stunting_agecat = kids %>% calcPtEst(var = 'stunted', by_var = 'age_cat',
                                     use_weights = TRUE, weight = 'svywt',
                                     strata = 'strata', psu = 'psu') 

p7 <- ggplot(stunting_agecat %>% filter(!is.na(age_cat)), aes(x = avg, y = age_cat, fill = avg)) + 
  geom_segment(aes(x = lb, xend = ub, y = age_cat, yend = age_cat), colour = grey15K, size = 2) +
  geom_point(size = 4, shape = 21, colour = grey90K) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) + 
  scale_fill_gradientn(colours = llamar::RdPu) +
  coord_flip() +
  xlab("Stunting rate") +
  ggtitle('Stunting rates by age category', subtitle = 'Source: DHS 2009') +
  theme_ygrid()
p7

write_csv(stunting_agecat, str_c(base_dir, out_dir, 'MDG_stunting_agecat.csv'))

# Check what files are saved for Tableau training
dir(str_c(base_dir, out_dir))


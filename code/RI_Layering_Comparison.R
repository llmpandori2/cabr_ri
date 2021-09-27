#################################################
# Title: Layering Comparison
# Purpose: examine differences between layered and un-layered data collection
#          using J. Burnaford FA20 CABR plots surveyed
# Author: LP
# Last edited: 2/16/2021
##################################################

##### Load packages and data #####

# packages 
library(tidyverse)
library(broom)
library(PNWColors)

# data
layer1 <- read_csv("Raw Data/PE_CAB220201214JLB - layers_active.csv")
layer2 <- read_csv("Raw Data/PE_CAB320201215JLB - layers_active.csv")
layer3 <- read_csv("Raw Data/PO_CAB120201214JLB - layers_active.csv")
  
single <- read_csv("Raw Data/CABR_photo_plot_layers_summary_active_20210216.csv")

##### Tidy Data #####

# make layer data summary data (% cover for each unique spp for each quadrat)
layers <- rbind(layer1, layer2, layer3)
remove(layer1, layer2, layer3)

layers2 <- layers %>%
  select(site_code:quadrat_code, top_layer:bottom_layer) %>%
  # convert wide to long (2 spp per place from top and bottom layers)
  pivot_longer(top_layer:bottom_layer, names_to = 'layer', values_to = 'species_code') %>%
  # get tally regardless of layer (// single data) %>%
  group_by(site_code, target_assemblage, quadrat_code, species_code) %>%
  tally()

# make // datasets to merge w/ single data
single2 <- single %>%
  select(site_code:quadrat_code, species_code, percent_cover)

# join datasets (keep all from both)
all <- full_join(layers2, single2)

##### Species Richness #####

# get spp richness in each quadrat (single)
single_richness <- single2 %>%
  group_by(site_code, target_assemblage, quadrat_code) %>%
  summarize(
    Single = length(unique(species_code))
  )

# get spp ricness in each quadrat (layer)
layer_richness <- layers2 %>%
  group_by(site_code, target_assemblage, quadrat_code) %>%
  summarize(
    Layer = length(unique(species_code))
  )

# merge & summarize for plotting


all_richness <- full_join(single_richness, layer_richness) %>%
  # convert wide to long
  pivot_longer(Single:Layer, names_to = 'Method', values_to = 'Richness') 
  
all_richness$site_code <- recode(all_richness$site_code, CAB1 = 'Zone 1', CAB2 = 'Zone 2', CAB3 = 'Zone 3')

all_richness$target_assemblage <- recode(all_richness$target_assemblage, pollicipes = 'Pollicipes', silvetia = 'Silvetia')

# summarize

all_richness_summary <- all_richness %>%
    ungroup() %>%
    group_by(site_code, target_assemblage, Method) %>%
    summarize(
      mean_richness = mean(Richness),
      sd_richness = sd(Richness),
      n = length(Richness)
    )
  
# Paired T-test to determine if spp richness for layer > single 
test1 <- t.test(Richness ~ Method, 
                data = filter(all_richness, site_code == 'CAB1'), paired = TRUE, 
                alternative = 'greater')
test2 <- t.test(Richness ~ Method, 
                data = filter(all_richness, site_code == 'CAB2'), paired = TRUE, 
                alternative = 'greater')
test3 <- t.test(Richness ~ Method, 
                data = filter(all_richness, site_code == 'CAB3'), paired = TRUE,
                alternative = 'greater')

# basic boxplot
ggplot(data = all_richness, 
       mapping = aes(x = Method, y = Richness)) + 
  geom_boxplot() + 
  facet_wrap(~site_code*target_assemblage) + 
  ggtitle('Photoplot Scoring Method - Diversity') + 
  ylab('Species Richness') + 
  theme_bw()+
  theme(text = element_text(size = 12),
        # make panels slightly farther apart
        panel.spacing = unit(1, 'lines'),
        # no background to wrap panels
        strip.background = element_blank(),
        strip.text = element_text(size = 12, color = 'black'),
        # panel labels outside x axis labels
        strip.placement = 'outside',
        legend.position = 'none',
        # tilt and adjust position of x axis labels
        axis.text.y = element_text(size = 12, color = 'black'),
        axis.text.x = element_text(size = 12, color = 'black'),
        legend.text = element_text(size = 12, color = 'black'),
        # make caption serif text to match journal style
        plot.caption = element_text(family = 'serif', 
                                    size = 12, color = 'black', hjust = 0)) 
  

# connected line plots (must pivot longer first)
wide_richness <- all_richness %>%
  # convert wide to long
  pivot_wider(names_from = 'Method', values_from = 'Richness')

wide_richness$site_code <- recode(wide_richness$site_code, CAB1 = 'Zone 1', CAB2 = 'Zone 2', CAB3 = 'Zone 3')

wide_richness$target_assemblage <- recode(wide_richness$target_assemblage, pollicipes = 'Pollicipes', silvetia = 'Silvetia')

wide_richness$quadrat_code <- as.factor(wide_richness$quadrat_code)

# Connected line graph
ggplot(data = wide_richness) + 
  geom_segment(mapping = aes(x = 1, xend = 2, y = Single, 
                             yend = Layer, color = quadrat_code)) +
  geom_point(mapping = aes(x = 1, y = Single, color = quadrat_code)) +
  geom_point(mapping = aes(x = 2, y = Layer, color = quadrat_code)) + 
  facet_wrap(~site_code*target_assemblage) +
  ggtitle('Species Richness by Layering Method') + 
  xlab('Method') + 
  ylab('Species Richness') + 
  coord_cartesian(xlim = c(0.75, 2.25)) + 
  scale_x_discrete(labels=c('1' = 'Single', '2' = 'Multiple')) +
  theme_bw()+
  theme(text = element_text(size = 12),
      # make panels slightly farther apart
      panel.spacing = unit(1, 'lines'),
      # no background to wrap panels
      strip.background = element_blank(),
      strip.text = element_text(size = 12, color = 'black'),
      # panel labels outside x axis labels
      strip.placement = 'outside',
      legend.position = 'none',
      # tilt and adjust position of x axis labels
      axis.text.y = element_text(size = 12, color = 'black'),
      axis.text.x = element_text(size = 12, color = 'black'),
      legend.text = element_text(size = 12, color = 'black'),
      # make caption serif text to match journal style
      plot.caption = element_text(family = 'serif', 
                                  size = 12, color = 'black', hjust = 0)) 



